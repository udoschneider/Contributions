"Filed out from Dolphin Smalltalk 7"!

Object subclass: #State
	instanceVariableNames: ''
	classVariableNames: 'AfterRoot InElement InProlog InStartTag'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
State guid: (GUID fromString: '{8b94676b-fcfe-406f-ab22-e19366f3934a}')!
State comment: 'This represents the state of a WAX object.Consider using an array of symbols instead of this class.Instance Variables'!
!State categoriesForClass!Unclassified! !
!State class methodsFor!

afterRoot	^AfterRoot!

inElement	^InElement!

initialize	"initializes class variables"	super initialize.	InProlog := State new.	InStartTag := State new.	InElement := State new.	AfterRoot := State new.!

inProlog	^InProlog!

inStartTag	^InStartTag! !
!State class categoriesFor: #afterRoot!accessing!public! !
!State class categoriesFor: #inElement!accessing!public! !
!State class categoriesFor: #initialize!initialize/release!public! !
!State class categoriesFor: #inProlog!accessing!public! !
!State class categoriesFor: #inStartTag!accessing!public! !

