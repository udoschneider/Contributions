"Filed out from Dolphin Smalltalk 7"!

ResourceChangingImageStripper subclass: #USImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
USImageStripper guid: (GUID fromString: '{7e74cccc-b81f-430a-b0e2-060f6c2a8e19}')!
USImageStripper comment: ''!
!USImageStripper categoriesForClass!Unclassified! !
!USImageStripper methodsFor!

stringTable: aStringTable at: key with: value 
	aStringTable at: key put: value!

stringTable: aStringTable replaceAt: key with: value 
	(aStringTable includesKey: key) ifTrue: [self stringTable: aStringTable at: key with: value ]!

versionResource
	| verString newVersionResource sessionManagerClass |
	newVersionResource := super versionResource.
	sessionManagerClass := self runtimeSessionManagerClass.
	verString := sessionManagerClass productVersionString.
	newVersionResource
		productVersion: verString;
		fileVersion: verString.
	newVersionResource stringTables do: 
			[:eachTable |
			self
				stringTable: eachTable
					replaceAt: 'LegalCopyright'
					with: sessionManagerClass legalCopyright;
				stringTable: eachTable
					replaceAt: 'ProductName'
					with: sessionManagerClass applicationName;
				stringTable: eachTable
					replaceAt: 'FileDescription'
					with: sessionManagerClass fileDescription;
				stringTable: eachTable
					replaceAt: 'Comments'
					with: sessionManagerClass comments;
				stringTable: eachTable
					at: 'ProjectEditionVersion'
					with: verString].
	^newVersionResource! !
!USImageStripper categoriesFor: #stringTable:at:with:!*-in class package!accessing!private! !
!USImageStripper categoriesFor: #stringTable:replaceAt:with:!*-in class package!accessing!private! !
!USImageStripper categoriesFor: #versionResource!*-in class package!accessing!private! !

