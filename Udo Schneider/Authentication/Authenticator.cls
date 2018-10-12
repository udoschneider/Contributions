"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Authenticator
	instanceVariableNames: 'failedLogins isLocked'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Authenticator guid: (GUID fromString: '{6e68deb6-6fa9-408e-825d-f4534e135ad3}')!
Authenticator comment: ''!
!Authenticator categoriesForClass!Kernel-Objects! !
!Authenticator methodsFor!

basicVerify: password 
self subclassResponsibility!

cacheCheck: password 
	^false!

currentTime
	^TimeStamp currentUTC!

initialize
	super initialize.
	isLocked := false.
	failedLogins := 0!

isLocked
	^isLocked!

lock
	isLocked := true!

onAuthenticationFailed: password timestamp: aTimeStamp 
	failedLogins := failedLogins + 1.
	failedLogins > 7 ifTrue: [self lock]. ^false!

onOuthenticationSuccess: password timestamp: aTimeStamp 
	failedLogins := 0.
	^true!

unlock
	isLocked := false!

verify: password 
	| result |
	
	[isLocked ifTrue: [^#locked -> (self onAuthenticationFailed: password timestamp: self currentTime)].
	(self cacheCheck: password) 
		ifTrue: [^#replay -> (self onAuthenticationFailed: password timestamp: self currentTime)].
		result := self basicVerify: password.
	(result ) value 
		ifFalse: [^#failed -> self onAuthenticationFailed: password timestamp: result key]
		ifTrue: [^#success -> self onOuthenticationSuccess: password timestamp: result key]] 
			on: Error
			do: [:ex | ^#error -> false]! !
!Authenticator categoriesFor: #basicVerify:!helpers!private! !
!Authenticator categoriesFor: #cacheCheck:!helpers!private! !
!Authenticator categoriesFor: #currentTime!helpers!private! !
!Authenticator categoriesFor: #initialize!initialization!private! !
!Authenticator categoriesFor: #isLocked!public!testing! !
!Authenticator categoriesFor: #lock!public! !
!Authenticator categoriesFor: #onAuthenticationFailed:timestamp:!helpers!private! !
!Authenticator categoriesFor: #onOuthenticationSuccess:timestamp:!helpers!private! !
!Authenticator categoriesFor: #unlock!public! !
!Authenticator categoriesFor: #verify:!public! !

!Authenticator class methodsFor!

new
	^super new initialize! !
!Authenticator class categoriesFor: #new!instance creation!public! !

