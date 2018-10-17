"Filed out from Dolphin Smalltalk 7"!

Object subclass: #NotifyAreaIcon
	instanceVariableNames: 'identifier message icon isVisible'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NotifyAreaIcon guid: (GUID fromString: '{6251907a-1962-47ec-b593-90d8664a2664}')!
NotifyAreaIcon comment: ''!
!NotifyAreaIcon categoriesForClass!Unclassified! !
!NotifyAreaIcon methodsFor!

balloon
^NotifyAreaBalloon new owner: self; yourself!

finalize

	self isVisible ifTrue: [self hide]!

hide
	self isVisible 
		ifTrue: 
			[NotifyAreaView default hide: self.
			isVisible := false. self beUnfinalizable ]!

icon
	^icon!

icon: anIcon 
	icon = anIcon 
		ifFalse: 
			[icon := anIcon.
			self update]!

identifier
	^identifier!

identifier: anObject
	identifier := anObject!

initialize
	identifier := nil.
	message := 'Notify message'.
	
	icon := Icon defaultApplication.
	isVisible := false!

isVisible
	^isVisible!

message
	^message!

message: aString 
message = aString ifFalse: [message  := aString . self update]!

show
	self isVisible 
		ifFalse: 
			[NotifyAreaView default add: self.
			isVisible := true. self beFinalizable ]!

showBalloonIcon: anIcon title: title message: aString flags: dwInfoFlags timeout: anInteger playSound: aBoolean 
	self isVisible 
		ifTrue: 
			[NotifyAreaView default 
				showBalloon: self
				icon: anIcon
				title: title
				message: aString
				flags: dwInfoFlags
				timeout: anInteger
				playSound: aBoolean]!

update
	self isVisible ifTrue: [NotifyAreaView default basicUpdate: self]! !
!NotifyAreaIcon categoriesFor: #balloon!public! !
!NotifyAreaIcon categoriesFor: #finalize!private! !
!NotifyAreaIcon categoriesFor: #hide!operations!public! !
!NotifyAreaIcon categoriesFor: #icon!accessing!public! !
!NotifyAreaIcon categoriesFor: #icon:!accessing!public! !
!NotifyAreaIcon categoriesFor: #identifier!accessing!public! !
!NotifyAreaIcon categoriesFor: #identifier:!accessing!private! !
!NotifyAreaIcon categoriesFor: #initialize!initialization!private! !
!NotifyAreaIcon categoriesFor: #isVisible!accessing!public!testing! !
!NotifyAreaIcon categoriesFor: #message!accessing!public! !
!NotifyAreaIcon categoriesFor: #message:!accessing!public! !
!NotifyAreaIcon categoriesFor: #show!operations!public! !
!NotifyAreaIcon categoriesFor: #showBalloonIcon:title:message:flags:timeout:playSound:!operations!public! !
!NotifyAreaIcon categoriesFor: #update!operations!private! !

!NotifyAreaIcon class methodsFor!

icon: anIcon message: aString 
	^(self new) icon: anIcon;
		message: aString;
		yourself!

new
 ^super new initialize! !
!NotifyAreaIcon class categoriesFor: #icon:message:!instance creation!public! !
!NotifyAreaIcon class categoriesFor: #new!instance creation!public! !

