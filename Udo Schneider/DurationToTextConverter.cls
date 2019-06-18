"Filed out from Dolphin Smalltalk 7"!

AbstractToTextConverter subclass: #DurationToTextConverter
	instanceVariableNames: 'terms'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DurationToTextConverter guid: (GUID fromString: '{d294d306-423d-4642-a9a1-1aa5626fa842}')!
DurationToTextConverter comment: ''!
!DurationToTextConverter categoriesForClass!TypeConverter-Text! !
!DurationToTextConverter methodsFor!

initialize
	super initialize.
	self terms: #('Year' 'Years' 'Day' 'Days' 'Minute' 'Minutes' 'Second' 'Seconds')!

leftToRight: aDuration 
	| stream days years |
	stream := ReadWriteStream on: String new.
	days := aDuration days.
	years := days // 365.
	days := days - (years * 365).
	years > 0 
		ifTrue: 
			[stream
				nextPutAll: years displayString;
				space;
				nextPutAll: (years > 1 ifTrue: [terms at: 2] ifFalse: [terms at: 1])].
	(years > 0 and: [days > 0]) ifTrue: [stream space].
	days > 0 
		ifTrue: 
			[stream
				nextPutAll: days displayString;
				space;
				nextPutAll: (days > 1 ifTrue: [terms at: 4] ifFalse: [terms at: 3])].
				
	(days > 0 and: [aDuration minutes > 0]) ifTrue: [stream space].
	aDuration minutes > 0 
		ifTrue: 
			[stream
				nextPutAll: aDuration minutes displayString;
				space;
				nextPutAll: (aDuration minutes > 1 ifTrue: [terms at: 6] ifFalse: [terms at: 5])].
	(aDuration minutes > 0 and: [aDuration seconds > 0]) ifTrue: [stream space].
	aDuration seconds > 0 
		ifTrue: 
			[stream
				nextPutAll: aDuration seconds displayString;
				space;
				nextPutAll: (aDuration seconds > 1 ifTrue: [terms at: 8] ifFalse: [terms at: 7])].
	^stream contents!

terms: anArray
	terms := anArray! !
!DurationToTextConverter categoriesFor: #initialize!public! !
!DurationToTextConverter categoriesFor: #leftToRight:!private! !
!DurationToTextConverter categoriesFor: #terms:!public! !

!DurationToTextConverter class methodsFor!

german
	^self terms: #('Jahr' 'Jahre' 'Tag' 'Tage' 'Minute' 'Minuten' 'Sekunde' 'Sekunden')!

terms: anArray 
	^ self new terms: anArray;
		 yourself! !
!DurationToTextConverter class categoriesFor: #german!public! !
!DurationToTextConverter class categoriesFor: #terms:!public! !

