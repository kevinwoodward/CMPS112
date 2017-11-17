#!/usr/bin/gst -f

nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

FileStream extend [
    nl := Character nl.

    populateFreqTable: table [

        [self atEnd not] whileTrue: [
            |ordChar|
            ordChar := self next asInteger.
            table incr: ordChar.
        ].
        table incr: 256.
    ]
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]


Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
"Execution begin"
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

debugFlag := false.
treeFlag := false.
compFlag := false.
uncompFlag := false.
hasFlag := false.
inFile := nil.
outFile := nil.
freqTable := ZeroArray new: 257.
sortedCol := SortedCollection new.
decodingTree := nil.
encodingTable := ZeroArray new: 257.

(Smalltalk arguments size < 2)
    ifTrue: ['Must have at least 2 arguments' printNl. ObjectMemory quit: 1.].

(1 to: Smalltalk arguments size)
do: [:index |
    arg := (Smalltalk arguments at: index).
    (arg = '-d') ifTrue: [debugFlag := true].
    hasFlag & ((arg = '-t') | (arg = '-c') | (arg = '-u'))
            ifTrue:
                ['Error' printNl.
                ObjectMemory quit: 1.]
            ifFalse:
                [(arg = '-t') ifTrue: [treeFlag := true. hasFlag := true.].
                (arg = '-c') ifTrue: [compFlag := true. hasFlag := true.].
                (arg = '-u') ifTrue: [uncompFlag := true. hasFlag := true.].
                (arg = '-td') | (arg = '-dt') ifTrue: [debugFlag := true. uncompFlag := true. hasFlag := true.].
                (arg = '-cd') | (arg = '-dc') ifTrue: [debugFlag := true. uncompFlag := true. hasFlag := true.].
                (arg = '-ud') | (arg = '-du') ifTrue: [debugFlag := true. uncompFlag := true. hasFlag := true.].
                ].
    (Smalltalk arguments size = 2)
        ifTrue: [
            (index = (Smalltalk arguments size))
                ifTrue: [inFile := arg.].
        ].
    (Smalltalk arguments size = 3)
        ifTrue: [
            (index = ((Smalltalk arguments size) - 1))
                ifTrue: [inFile := arg.].
            (index = (Smalltalk arguments size))
                ifTrue: [outFile := arg.].
        ].
].

(hasFlag not)
    ifTrue: ['Must specify -t, -c, or -u.' printNl. ObjectMemory quit: 1.].

inStream := FileStream open: inFile
                     mode: FileStream read.

(compFlag | treeFlag)
ifTrue: [
    inStream populateFreqTable: freqTable.

    freqTable keysAndValuesDo: [:key :value |
       (value notNil and: [value > 0]) ifTrue: [
          "stdout << 'freq[' << key << ']=' << value << nl."
          sortedCol add: (Leaf new: key count: value).
       ]
    ].

    [sortedCol size = 1]
    whileFalse: [
        |elem1 elem2 tChar tCount|
        elem1 := sortedCol removeFirst.
        elem2 := sortedCol removeFirst.
        tChar := elem1 char.
        tCount := (elem1 count + elem2 count).
        sortedCol add: (Tree new: tChar
                            count: tCount
                            left: elem1
                            right: elem2).
    ].

    decodingTree := sortedCol removeFirst.

    decodingTree inorder: [:c :s |
            encodingTable at: (c asInteger) put: s.
    ] prefix: ''.

    (treeFlag)
        ifTrue: [
            encodingTable keysAndValuesDo: [:key :value |
               (value notNil and: [value > 0]) ifTrue: [
                  stdout << 'encoding[' << key << ']=' << value << nl.
               ]
            ].
        ].
].
