"Filed out from Dolphin Smalltalk 7"!

GdiplusInitializer subclass: #GdiplusBitmapFromRawInitializer
	instanceVariableNames: 'width height stride format scan'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GdiplusBitmapFromRawInitializer guid: (GUID fromString: '{9368ee40-c3a0-4a20-ba45-0f4419de569d}')!
GdiplusBitmapFromRawInitializer comment: ''!
!GdiplusBitmapFromRawInitializer categoriesForClass!Unclassified! !
!GdiplusBitmapFromRawInitializer methodsFor!

createHandle
	" Bitmap::Bitmap(
	    IN INT width,
	    IN INT height,
	    IN PixelFormat format
	    )
	{
	    GpBitmap *bitmap = NULL;

	    lastResult = DllExports::GdipCreateBitmapFromScan0(width,
                                                       height,
                                                       stride,
                                                       format,
                                                       scan0,
                                                       &bitmap);"

	| gpHandle status |
	gpHandle := ExternalHandle new.
	status := self library
				gdipCreateBitmapFromScan0: width
				height: height
				stride: stride
				format: format
				scan0: scan asParameter
				bitmap: gpHandle.
			status == Ok ifFalse: [self gdiplusErrorWith: status].
	^gpHandle!

gdiplusConstructorErrorDescription
	^'Error creating Bitmap'!

width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress 
	
	width := widthInteger.
	height := heightInteger.
	stride := strideInteger.
	format := aPixelFormat.
	scan := anExternalAddress! !
!GdiplusBitmapFromRawInitializer categoriesFor: #createHandle!public!realizing/unrealizing! !
!GdiplusBitmapFromRawInitializer categoriesFor: #gdiplusConstructorErrorDescription!constants!private! !
!GdiplusBitmapFromRawInitializer categoriesFor: #width:height:stride:format:scan:!accessing!public! !

!GdiplusBitmapFromRawInitializer class methodsFor!

width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress 
	^self new 
	width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress ! !
!GdiplusBitmapFromRawInitializer class categoriesFor: #width:height:stride:format:scan:!instance creation!public! !

