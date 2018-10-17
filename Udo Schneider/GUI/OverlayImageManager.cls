"Filed out from Dolphin Smalltalk 7"!

ImageManager subclass: #OverlayImageManager
	instanceVariableNames: 'overlayImages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OverlayImageManager guid: (GUID fromString: '{c0c00a3a-d476-4d94-b2e5-77dbf8f1384f}')!
OverlayImageManager comment: ''!
!OverlayImageManager categoriesForClass!Unclassified! !
!OverlayImageManager methodsFor!

addOverlayImage: anImage 
	(overlayImages includes: anImage) ifFalse: [self basicAddOverlayImage: anImage].
	^overlayImages indexOf: anImage!

basicAddOverlayImage: anImage 
	| imageIndex overlayIndex |
	imageIndex := self indexOfImage: anImage.
	overlayImages addLast: anImage.
	overlayIndex := overlayImages indexOf: anImage.
	imageLists do: [:hImageList | hImageList use: imageIndex asOverlay: overlayIndex]!

buildImageListWithExtent: aPoint 
	| newList |
	newList := super buildImageListWithExtent: aPoint.
	overlayImages 
		keysAndValuesDo: [:overlayIndex :overlayImage | newList use: (self indexOfImage: overlayImage) asOverlay: overlayIndex].
	^newList!

indexOfOverlayImage: anImage 
^anImage isNil ifTrue: [0] ifFalse: [self addOverlayImage: anImage]
	!

initialize
	overlayImages := OrderedCollection new.
	^super initialize!

newImageListWithExtent: aPoint 
	"Private - Answer a new <WinImageList> appropriately configured to hold images for the receiver
	of the specified extent."

	| answer |
	(aPoint x <= 0 or: [aPoint y <= 0]) ifTrue: [^self error: 'Image list extent must be at least 1@1'].
	answer := WinImageList 
				newExtent: aPoint
				initialSize: images size
				masked: true.
	answer backcolor: backcolor.
	^answer!

purgeImages
	"Private - Frees up all the image resources"


	self newImageLists! !
!OverlayImageManager categoriesFor: #addOverlayImage:!adding!public! !
!OverlayImageManager categoriesFor: #basicAddOverlayImage:!adding!private! !
!OverlayImageManager categoriesFor: #buildImageListWithExtent:!helpers!private! !
!OverlayImageManager categoriesFor: #indexOfOverlayImage:!public!searching! !
!OverlayImageManager categoriesFor: #initialize!initializing!private! !
!OverlayImageManager categoriesFor: #newImageListWithExtent:!helpers!private! !
!OverlayImageManager categoriesFor: #purgeImages!operations!private! !

