"Filed out from Dolphin Smalltalk 7"!

AbstractToTextConverter subclass: #CollectionToText
	instanceVariableNames: 'elementTypeconverter printSeparator parseSeparator'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CollectionToText guid: (GUID fromString: '{aad7a8b3-0ba3-4785-b902-6f4fd580c110}')!
CollectionToText comment: ''!
!CollectionToText categoriesForClass!MVP-Type Converters-Abstract!MVP-Type Converters-General!MVP-Type Converters-Text! !
!CollectionToText methodsFor!

elementTypeconverter
	^elementTypeconverter!

elementTypeconverter: anObject 
	elementTypeconverter := anObject!

isLeftNullValue: anObject
	^anObject isNilOrEmpty!

leftToRight: anObject 
	| stream |
	stream := ReadWriteStream on: String new.
	anObject do: [:each | stream nextPutAll: (elementTypeconverter convertFromLeftToRight: each)]
		separatedBy: [stream nextPutAll: printSeparator].
	^stream contents!

rightToLeft: aString 
	| elements lastFoundPosition nextFoundPosition |
	aString isNilOrEmpty ifTrue: [^self leftNullValue ].
	elements := OrderedCollection new.
	lastFoundPosition := 0.
	[(nextFoundPosition := aString findString: parseSeparator startingAt: lastFoundPosition + 1) > 0] 
		whileTrue: 
			[elements add: (aString copyFrom: lastFoundPosition + 1 to: nextFoundPosition - 1).
			lastFoundPosition := nextFoundPosition + parseSeparator size - 1].
	lastFoundPosition < aString size 
		ifTrue: [elements add: (aString copyFrom: lastFoundPosition + 1 to: aString size)].
	^elements collect: [:each | elementTypeconverter rightToLeft: each trimBlanks]!

setPintSeparator: print parseSeparator: parse elementConverter: aTypeConverter 
	printSeparator := print.
	parseSeparator := parse.
	elementTypeconverter := aTypeConverter! !
!CollectionToText categoriesFor: #elementTypeconverter!accessing!public! !
!CollectionToText categoriesFor: #elementTypeconverter:!accessing!public! !
!CollectionToText categoriesFor: #isLeftNullValue:!public! !
!CollectionToText categoriesFor: #leftToRight:!private! !
!CollectionToText categoriesFor: #rightToLeft:!private! !
!CollectionToText categoriesFor: #setPintSeparator:parseSeparator:elementConverter:!private! !

!CollectionToText class methodsFor!

applicableTypeConverterCategories
	"Answers a class category containing <typeConverter>s that can 
    	be used with the receiver"

	^Set with: (ClassCategory name: 'MVP-Type Converters-General') with: (ClassCategory name: 'MVP-Type Converters-Text')
		!

elementTypeconverter: aTypeConverter 
	^self 
		printSeparator: ', '
		parseSeparator: ','
		elementConverter: aTypeConverter!

new
	^ self 
		printSeparator: ', '
		parseSeparator: ','
		elementConverter: NullConverter new!

printSeparator: print parseSeparator: parse elementConverter: aTypeConverter 
	^ super new
		setPintSeparator: print
			parseSeparator: parse
			elementConverter: aTypeConverter;
		yourself!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| applicableTypeConverters |
	applicableTypeConverters := Set new.
	self applicableTypeConverterCategories do: [:each | applicableTypeConverters addAll: each contents].
	^(super publishedAspectsOfInstances)
		add: (Aspect name: #elementTypeconverter
					chooseFrom: (applicableTypeConverters asSortedCollection asArray 
							collect: [:each | each name , ' new'])) 
						beImmutable;
		yourself!

separator: separator elementConverter: aTypeConverter 
	^ self
		printSeparator: separator
		parseSeparator: separator
		elementConverter: aTypeConverter! !
!CollectionToText class categoriesFor: #applicableTypeConverterCategories!constants!public! !
!CollectionToText class categoriesFor: #elementTypeconverter:!public! !
!CollectionToText class categoriesFor: #new!public! !
!CollectionToText class categoriesFor: #printSeparator:parseSeparator:elementConverter:!public! !
!CollectionToText class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!CollectionToText class categoriesFor: #separator:elementConverter:!public! !

