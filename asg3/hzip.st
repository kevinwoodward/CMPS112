#!/usr/bin/gst -f

nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger.]
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

    writeEncodedData: table stream: stream [
        [self atEnd not] whileTrue: [
            |ordChar|
            ordChar := self next asInteger.
            1 to: ((table at: ordChar) size) do:
                [:index| stream writeBit: (((table at: ordChar) at: index) asInteger)].
        ].

        1 to: ((table at: 256) size) do:
            [:index| stream writeBit: (((table at: 256) at: index) asInteger)].
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
      ^ aStream << self class << '(' << char asCharacter visible << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

    postorder: bitstream [
        bitstream writeBit: 0.
        8 to: 1 by: -1 do: [:index|
            bitstream writeBit: (char bitAt: index).
        ].
        (char = 0)
        ifTrue: [
            bitstream writeBit: 0.
        ].
        (char = 256)
        ifTrue: [
            bitstream writeBit: 1.
        ].
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

   postorder: bitstream [
        left postorder: bitstream.
        right postorder: bitstream.
        bitstream writeBit: 1
   ]

   left [^left]
   right [^right]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.

   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]

   currByte [
      ^byte
   ]

   flushByte [
      bitIndex = 9 ifTrue: [
         myStream nextPutByte: byte.
         byte := 0.
         bitIndex := 1.

      ]
   ]
   writeBit: bit [
      (byte = nil) ifTrue: [byte := 0].
      byte := byte bitAt: (9 - bitIndex) put: bit.
      bitIndex := bitIndex + 1.
      self flushByte.
   ]
   clearByte [
      bitIndex := 8.
      byte := 0.
   ]
   padBits [
        [bitIndex = 1]
            whileFalse: [
                self writeBit: 0.
            ].
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
encodingTree := nil.
encodingTable := ZeroArray new: 257.
savedFinal := nil.

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


(outFile = nil)
    ifTrue: [outStream := stdout.]
    ifFalse: [outStream := FileStream open: outFile
                         mode: FileStream write.].

outBitstream := BitStream new: outStream.


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

    encodingTree := sortedCol removeFirst.

    encodingTree inorder: [:c :s |
            encodingTable at: (c asInteger) put: s.
    ] prefix: ''.

    (treeFlag)

        ifTrue: [
            encodingTable keysAndValuesDo: [:key :value |
               (value notNil and: [value > 0]) ifTrue: [
                    |charStr freq|
                    freq := freqTable at: key.
                    charStr := key asCharacter visible.
                    charStr asString size to: 3 do: [:skip| stdout << ' '].
                    stdout << ' ' << charStr.
                    freq asString size to: 5 do: [:skip| stdout << ' '].
                    stdout << ' ' << freq << ' ' << value << nl.
               ].
            ].
            ObjectMemory quit: 0.
        ].
    (compFlag)
        ifTrue:[
            |encodingStr|
            inStream reset.
            encodingTree postorder: outBitstream.
            inStream writeEncodedData: encodingTable stream: outBitstream.
            outBitstream padBits.
            ObjectMemory quit: 0.
        ].
].

carryBit := nil.

(uncompFlag)
ifTrue: [
    treeStack := OrderedCollection new.
    inBitstream := BitStream new: inStream.
    doneDecodingTree := false.
    [(inBitstream atEnd not) & (doneDecodingTree not)]
        whileTrue:
        [ |currBit currByte|
            currBit := inBitstream nextBit.
            carryBit := currBit.
            currByte := 0.
            binaryConverter := 128.
            (currBit = 0)
                ifTrue:
                [
                    (1 to: 8) do:
                    [:useless|
                        currBit := inBitstream nextBit.
                        carryBit := currBit.
                        (currBit = 1)
                            ifTrue:
                            [
                                currByte := currByte + binaryConverter.
                            ].
                            binaryConverter := binaryConverter / 2.
                    ].
                    (currByte = 0)
                        ifTrue:
                        [ |tempBit|
                            tempBit := inBitstream nextBit.
                            carryBit := currBit.
                            (tempBit = 1)
                                ifTrue:
                                [
                                    currByte := currByte + 256.
                                ].


                        ].
                    treeStack addFirst: (Leaf new: currByte count: 1).
                ]
            ifFalse:
                [ |rightTree leftTree tempTree|
                    rightTree := treeStack removeFirst.
                    (treeStack size = 0)
                        ifTrue:
                        [
                            savedFinal := rightTree.
                            doneDecodingTree := true.
                        ]
                        ifFalse:
                        [
                            leftTree := treeStack removeFirst.
                            treeStack addFirst: (Tree new: (leftTree char) count: 1 left: leftTree right: rightTree). "MAYBE WRONG"
                        ].

                ].
        ].



    tempTree := savedFinal.
    eof := false.
    [(inBitstream atEnd not) & (eof not)]
    whileTrue:
    [
        [tempTree isMemberOf: Leaf]
            whileFalse:
                [|tbit|
                (carryBit = nil)
                    ifTrue:
                    [
                        tbit := inBitstream nextBit.
                    ]
                    ifFalse:
                    [
                        tbit := carryBit.
                        carryBit := nil.
                    ].

                (tbit = 0)
                    ifTrue:
                    [
                        tempTree := (tempTree left).
                    ]
                    ifFalse:
                    [
                        tempTree := (tempTree right).
                    ].
                ].
                tempTree char printNl.
                (tempTree char = 256)
                    ifTrue:[eof := true.]
                    ifFalse:[outStream << tempTree char asCharacter.].

                tempTree := savedFinal.
    ].


].
