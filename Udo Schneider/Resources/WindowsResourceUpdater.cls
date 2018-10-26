"Filed out from Dolphin Smalltalk 7"!

Object subclass: #WindowsResourceUpdater
	instanceVariableNames: 'filename'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
WindowsResourceUpdater guid: (GUID fromString: '{69889cc6-33ef-4c78-8d7a-790dc8d64763}')!
WindowsResourceUpdater comment: ''!
!WindowsResourceUpdater categoriesForClass!Unclassified! !
!WindowsResourceUpdater methodsFor!

closeLibraries
	(ExternalResourceLibrary allInstances select: [:each | each fileName = filename asLowercase]) 
		do: [:each | each close]!

filename
	^filename!

filename: anObject
	filename := anObject!

initialize!

openLibraryWhile: aBlock 
	| lib |
	lib := ExternalResourceLibrary open: filename.
	aBlock value: lib  . lib close!

removeResources
	| toDelete |
	self
		openLibraryWhile: 
				[:lib |
				toDelete := Set new.
				self supportedResourceTypes
					do: [:resourceType | toDelete addAll: (lib resourceIdentifiersOfType: resourceType)]];
		closeLibraries;
		removeResources: toDelete!

removeResources: aCollectionOfResourceIdentifiers
	self updateResourcesWhile: 
			[:hUpdate |
			aCollectionOfResourceIdentifiers do: 
					[:each |
					KernelLibrary default
						deleteResource: hUpdate
						lpType: each type asResourceType
						lpName: each id asResourceId
						wLanguage: each locale asParameter]]!

supportedResourceTypes
	^Array 
		with: RT_ICON
		with: RT_GROUP_ICON
		with: RT_CURSOR
		with: RT_GROUP_CURSOR
		with: RT_BITMAP!

updateResourcesWhile: aBlock

	KernelLibrary default updateResourcesOf: filename
		do: 
			aBlock! !
!WindowsResourceUpdater categoriesFor: #closeLibraries!accessing!public! !
!WindowsResourceUpdater categoriesFor: #filename!accessing!public! !
!WindowsResourceUpdater categoriesFor: #filename:!accessing!private! !
!WindowsResourceUpdater categoriesFor: #initialize!public! !
!WindowsResourceUpdater categoriesFor: #openLibraryWhile:!accessing!public! !
!WindowsResourceUpdater categoriesFor: #removeResources!accessing!public! !
!WindowsResourceUpdater categoriesFor: #removeResources:!accessing!public! !
!WindowsResourceUpdater categoriesFor: #supportedResourceTypes!accessing!public! !
!WindowsResourceUpdater categoriesFor: #updateResourcesWhile:!accessing!public! !

!WindowsResourceUpdater class methodsFor!

new
^super new initialize!

onFile: aFilename
^self new filename: aFilename! !
!WindowsResourceUpdater class categoriesFor: #new!public! !
!WindowsResourceUpdater class categoriesFor: #onFile:!public! !

