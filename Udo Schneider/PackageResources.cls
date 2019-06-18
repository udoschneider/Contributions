"Filed out from Dolphin Smalltalk 7"!

Object subclass: #PackageResources
	instanceVariableNames: 'package packageLocator'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PackageResources guid: (GUID fromString: '{008ef580-73cb-4d1e-87fc-fd43abd1ec36}')!
PackageResources comment: ''!
!PackageResources categoriesForClass!Unclassified! !
!PackageResources methodsFor!

bitmap: anIdentifier 
	^DIBSection fromFile: anIdentifier usingLocator: self packageLocator!

icon: anIdentifier
	^Icon
		fromId: anIdentifier
		in: nil
		usingLocator: self packageLocator!

localFileSpecFor: aString 
	^self packageLocator localFileSpecFor: aString!

olePicture: anIdentifier 
	^OLEPicture fromFile: anIdentifier usingLocator: self packageLocator!

package
	^package!

packageLocator
	packageLocator isNil
		ifTrue: [packageLocator := PackageFolderRelativeFileLocator package: self package folder: 'Resources'].
	^packageLocator!

setPackage: aPackage
	package := aPackage!

sound: anIdentifier
	^Sound fromFile: anIdentifier usingLocator: self packageLocator! !
!PackageResources categoriesFor: #bitmap:!public! !
!PackageResources categoriesFor: #icon:!public! !
!PackageResources categoriesFor: #localFileSpecFor:!public! !
!PackageResources categoriesFor: #olePicture:!public! !
!PackageResources categoriesFor: #package!accessing!public! !
!PackageResources categoriesFor: #packageLocator!public! !
!PackageResources categoriesFor: #setPackage:!accessing!private! !
!PackageResources categoriesFor: #sound:!public! !

!PackageResources class methodsFor!

forPackage: aPackage
	^super new setPackage: aPackage; yourself!

forPackageNamed: aString
	^self forPackage: (Package manager packageNamed: aString)!

new
	^self shouldNotImplement! !
!PackageResources class categoriesFor: #forPackage:!instance creation!public! !
!PackageResources class categoriesFor: #forPackageNamed:!instance creation!public! !
!PackageResources class categoriesFor: #new!instance creation!public! !

