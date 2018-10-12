"Filed out from Dolphin Smalltalk 7"!

Authenticator subclass: #OneTimePassword
	instanceVariableNames: 'usedOtps'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OneTimePassword guid: (GUID fromString: '{ff42cdf2-0d48-47b9-9fba-5bc5c6cc2a90}')!
OneTimePassword comment: ''!
!OneTimePassword categoriesForClass!Kernel-Objects! !
!OneTimePassword methodsFor!

allowedDeviation
	"+/- Deviation in minutes of allowed Password"

	self subclassResponsibility!

basicVerify: password 
	self subclassResponsibility!

basicVerify: aString time: otpTime 
	self subclassResponsibility!

cacheCheck: password 
	self cacheClean.
	^usedOtps anySatisfy: [:each | each value = password]!

cacheClean
	| timestamp |
	timestamp := TimeStamp fromSeconds: self currentTime asSeconds - (self cacheTimeout * 60).
	usedOtps := usedOtps reject: [:each | each key < timestamp]!

cacheTimeout
	"in minutes"

	^5!

calculateOtp
	^self calculateOtp: self otpTime!

calculateOtp: otpTime 
	self subclassResponsibility!

initialize
	super initialize.
	usedOtps := Set new.
!

onAuthenticationFailed: password timestamp: aTimeStamp 
	usedOtps add: aTimeStamp -> password.
	^super onAuthenticationFailed: password timestamp: aTimeStamp!

onOuthenticationSuccess: password timestamp: aTimeStamp 
	usedOtps add: aTimeStamp -> password.
	^super onOuthenticationSuccess: password timestamp: aTimeStamp!

otpTime
	self subclassResponsibility!

otpTime: otpTime generates: password 
	^(self calculateOtp: otpTime) = password!

otpTimeFromTimeStamp: aTimeStamp 
	self subclassResponsibility!

timeStampFromOtpTime: ticks 
	self subclassResponsibility! !
!OneTimePassword categoriesFor: #allowedDeviation!constants!public! !
!OneTimePassword categoriesFor: #basicVerify:!helpers!private! !
!OneTimePassword categoriesFor: #basicVerify:time:!helpers!private! !
!OneTimePassword categoriesFor: #cacheCheck:!helpers!private! !
!OneTimePassword categoriesFor: #cacheClean!helpers!private! !
!OneTimePassword categoriesFor: #cacheTimeout!constants!public! !
!OneTimePassword categoriesFor: #calculateOtp!public! !
!OneTimePassword categoriesFor: #calculateOtp:!helpers!private! !
!OneTimePassword categoriesFor: #initialize!initialization!private! !
!OneTimePassword categoriesFor: #onAuthenticationFailed:timestamp:!helpers!private! !
!OneTimePassword categoriesFor: #onOuthenticationSuccess:timestamp:!helpers!private! !
!OneTimePassword categoriesFor: #otpTime!helpers!private! !
!OneTimePassword categoriesFor: #otpTime:generates:!helpers!private! !
!OneTimePassword categoriesFor: #otpTimeFromTimeStamp:!helpers!private! !
!OneTimePassword categoriesFor: #timeStampFromOtpTime:!helpers!private! !

