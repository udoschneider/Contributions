"Filed out from Dolphin Smalltalk 7"!

GdiplusInitializer subclass: #GdiplusRegionFromRegionInitializer
	instanceVariableNames: 'region'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GdiplusRegionFromRegionInitializer guid: (GUID fromString: '{1f63ee28-03ba-4bc5-98ef-122781f6f109}')!
GdiplusRegionFromRegionInitializer comment: ''!
!GdiplusRegionFromRegionInitializer categoriesForClass!Unclassified! !
!GdiplusRegionFromRegionInitializer methodsFor!

createHandle
	| gpHandle status |
	gpHandle := ExternalHandle new.
		status := self library gdipCreateRegionHrgn: region asParameter region: gpHandle.
	
	status == Ok ifFalse: [self gdiplusErrorWith: status].
	^gpHandle!

gdiplusConstructorErrorDescription
	^'Error creating Region'!

region: aRegion 
	region := aRegion! !
!GdiplusRegionFromRegionInitializer categoriesFor: #createHandle!public!realizing/unrealizing! !
!GdiplusRegionFromRegionInitializer categoriesFor: #gdiplusConstructorErrorDescription!constants!private! !
!GdiplusRegionFromRegionInitializer categoriesFor: #region:!accessing!public! !

!GdiplusRegionFromRegionInitializer class methodsFor!

fromRegion: aRegion 
	^self new region: aRegion! !
!GdiplusRegionFromRegionInitializer class categoriesFor: #fromRegion:!instance creation!public! !

