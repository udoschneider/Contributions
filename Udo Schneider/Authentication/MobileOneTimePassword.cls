"Filed out from Dolphin Smalltalk 7"!

OneTimePassword subclass: #MobileOneTimePassword
	instanceVariableNames: 'secret pin tokenTimeDeviation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MobileOneTimePassword guid: (GUID fromString: '{332974a3-f52d-4b4a-a689-3f51bb62035d}')!
MobileOneTimePassword comment: ''!
!MobileOneTimePassword categoriesForClass!Kernel-Objects! !
!MobileOneTimePassword methodsFor!

allowedDeviation
	"+/- Deviation in minutes of allowed Password"

	^3!

basicVerify: password 
	^self basicVerify: password time: self otpTime + tokenTimeDeviation!

basicVerify: aString time: otpTime 
	| password deviationTime result |
	password := aString asLowercase.
	deviationTime := self allowedDeviation * 6.
	result := (self timeStampFromOtpTime: otpTime) -> false.
	(otpTime - deviationTime to: otpTime + deviationTime) detect: 
			[:time | 
			(result := (self timeStampFromOtpTime: time) -> (self otpTime: time generates: password)) value]
		ifNone: [].
	^result!

cacheTimeout
	"in minutes"

	^5!

calculateOtp: otpTime 
	| md5 hashData |
	hashData := otpTime displayString , secret , pin.
	md5 := self md5: hashData.
	^md5 copyFrom: 1 to: 6!

initialize
	super initialize.
	tokenTimeDeviation := 0!

md5: hashData 
	^((MD5 hashMessage: hashData) printStringRadix: 16 showRadix: false) asLowercase!

otpTime
	^self otpTimeFromTimeStamp: self currentTime!

otpTimeFromTimeStamp: aTimeStamp 
	^aTimeStamp asUnixEpocheSeconds // 10!

setPassphrase: aPassphraseString pin: aPinString 
	self setSecret: ((self md5: aPassphraseString) copyFrom: 1 to: 16) pin: aPinString!

setSecret: aSecretString pin: aPinString
secret := aSecretString.
pin := aPinString!

sync: password otpTime: otpTokenTime 
	| result |
	result := self basicVerify: password time: otpTokenTime.
	result value ifTrue: [tokenTimeDeviation := otpTokenTime - self otpTime].
	^result value!

timeStampFromOtpTime: ticks 
	^TimeStamp fromUnixEpocheSeconds: ticks * 10! !
!MobileOneTimePassword categoriesFor: #allowedDeviation!constants!public! !
!MobileOneTimePassword categoriesFor: #basicVerify:!helpers!private! !
!MobileOneTimePassword categoriesFor: #basicVerify:time:!helpers!private! !
!MobileOneTimePassword categoriesFor: #cacheTimeout!constants!public! !
!MobileOneTimePassword categoriesFor: #calculateOtp:!helpers!private! !
!MobileOneTimePassword categoriesFor: #initialize!initialization!private! !
!MobileOneTimePassword categoriesFor: #md5:!helpers!private! !
!MobileOneTimePassword categoriesFor: #otpTime!helpers!private! !
!MobileOneTimePassword categoriesFor: #otpTimeFromTimeStamp:!helpers!private! !
!MobileOneTimePassword categoriesFor: #setPassphrase:pin:!initialization!private! !
!MobileOneTimePassword categoriesFor: #setSecret:pin:!initialization!private! !
!MobileOneTimePassword categoriesFor: #sync:otpTime:!public! !
!MobileOneTimePassword categoriesFor: #timeStampFromOtpTime:!helpers!private! !

!MobileOneTimePassword class methodsFor!

passphrase: passphrase pin: pin 
	^(self new )
		setPassphrase: passphrase pin: pin;
		yourself!

secret: secret pin: pin 
	"secret: init-secred from token (to init token: #**#)
	pin: User Pin"

	^(self  new )
		setSecret: secret pin: pin;
		yourself! !
!MobileOneTimePassword class categoriesFor: #passphrase:pin:!instance creation!public! !
!MobileOneTimePassword class categoriesFor: #secret:pin:!instance creation!public! !

