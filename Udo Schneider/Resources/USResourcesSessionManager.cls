"Filed out from Dolphin Smalltalk 7"!

RuntimeSessionManager subclass: #USResourcesSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
USResourcesSessionManager guid: (GUID fromString: '{65d251ec-633c-4ffa-9851-82b8b5c77535}')!
USResourcesSessionManager comment: ''!
!USResourcesSessionManager categoriesForClass!System-Support! !
!USResourcesSessionManager methodsFor!

main
	self mainShellClass show! !
!USResourcesSessionManager categoriesFor: #main!operations!public! !

!USResourcesSessionManager class methodsFor!

applicationName
	^'Resource Changing Sample Application'!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter)."

	^USResourcesSampleShell!

productVersion
	^'0.1alpha'! !
!USResourcesSessionManager class categoriesFor: #applicationName!public! !
!USResourcesSessionManager class categoriesFor: #mainShellClass!constants!public! !
!USResourcesSessionManager class categoriesFor: #productVersion!public! !

