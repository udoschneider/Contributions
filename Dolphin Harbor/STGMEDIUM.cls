"Filed out from Dolphin Smalltalk 7"!

OLEFinalizableStructure subclass: #STGMEDIUM
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'OLEShellDataTransferConstants'
	classInstanceVariableNames: ''!
STGMEDIUM guid: (GUID fromString: '{a151b16b-b8fe-4851-9bb2-e17202681e82}')!
STGMEDIUM comment: 'STGMEDIUM is an <ExternalStructure> that wraps the Windows STGMEDIUM structure.  The STGMEDIUM structure is a generalized global memory handle used for data transfer operations by the IAdviseSink, IDataObject, and IOleCache interfaces.

I need finalization

[Edit the following which is almost verbatim from MSDN]

- tymed, the type of storage medium. The marshaling and unmarshaling routines use this value to determine which union member was used. This value must be one of the elements of the TYMED enumeration. 
union member 

- Handle, string, or interface pointer that the receiving process can use to access the data being transferred. If tymed is TYMED_NULL, the union member is undefined; otherwise, it is one of the following:
	hBitmap, a Bitmap handle. The tymed member is TYMED_GDI.
	hMetaFilePict, a Metafile handle. The tymed member is TYMED_MFPICT.
	hEnhMetaFile, an Enhanced metafile handle. The tymed member is TYMED_ENHMF.
	hGlobal, a Global memory handle. The tymed member is TYMED_HGLOBAL.
	lpszFileName, a pointer to the path of a disk file that contains the data. The tymed member is TYMED_FILE.
	pstm, a pointer to an IStream interface. The tymed member is TYMED_ISTREAM.
	pstg, a pointer to an IStorage interface. The tymed member is TYMED_ISTORAGE.

- pUnkForRelease, a pointer to an interface instance that allows the sending process to control the way the storage is released when the receiving process calls the ReleaseStgMedium function. If pUnkForRelease is NULL, ReleaseStgMedium uses default procedures to release the storage; otherwise, ReleaseStgMedium uses the specified IUnknown interface. 
'!
!STGMEDIUM categoriesForClass!Unclassified! !
!STGMEDIUM methodsFor!

basicFree
	"Private - Free external resources owned by the receiver.
	MSDN:
		The provider indicates that the receiver of the medium is responsible for freeing
		 the medium by specifying NULL for the punkForRelease structure member,
"

	self pUnkForRelease isNull ifTrue: [OLELibrary default releaseStgMedium: self]!

getValidFields
	"Private - Answer a <sequencedReadableCollection> of the fields defined 
	in the receiver's template, sorted in ascending order of offset from the start of the
	structure, that are valid in this particular instance."

	^#(#tymed)!

hBitmap
	"Answer the receiver's hBitmap field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalHandle!

hBitmap: anObject
	"Set the receiver's hBitmap field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

hEnhMetaFile
	"Answer the receiver's hEnhMetaFile field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalHandle!

hEnhMetaFile: anObject
	"Set the receiver's hEnhMetaFile field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

hGlobal
	"Answer the receiver's hGlobal field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalHandle!

hGlobal: anObject
	"Set the receiver's hGlobal field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

hMetaFilePict
	"Answer the receiver's hMetaFilePict field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalHandle!

hMetaFilePict: anObject
	"Set the receiver's hMetaFilePict field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

lpszFileName
	"Answer the receiver's lpszFileName field as a Smalltalk object."

	^UnicodeString fromAddress: (bytes sdwordAtOffset: 4)!

lpszFileName: anObject
	"Set the receiver's lpszFileName field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject yourAddress!

medium
	self tymed = 0 ifTrue: [^nil].
	^self perform: (self mediums at: self tymed)!

mediums
	| dict |
	(dict := IdentityDictionary new)
		at: TYMED_HGLOBAL put: #hGlobal;
		at: TYMED_FILE put: #lpszFileName;
		at: TYMED_ISTREAM put: #pstm;
		at: TYMED_ISTORAGE put: #pstg;
		at: TYMED_GDI put: #hBitmap;
		at: TYMED_MFPICT put: #hMetaFilePict;
		at: TYMED_ENHMF put: #hEnhMetaFile.
	^dict!

pstg
	"Answer the receiver's pstg field as a Smalltalk object."

	^IStorage fromAddress: (bytes sdwordAtOffset: 4)!

pstg: anObject
	"Set the receiver's pstg field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject yourAddress!

pstm
	"Answer the receiver's pstm field as a Smalltalk object."

	^IStream fromAddress: (bytes sdwordAtOffset: 4)!

pstm: anObject
	"Set the receiver's pstm field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject yourAddress!

pUnkForRelease
	"Answer the receiver's pUnkForRelease field as a Smalltalk object."

	^IUnknown fromAddress: (bytes sdwordAtOffset: 8)!

pUnkForRelease: anObject
	"Set the receiver's pUnkForRelease field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject yourAddress!

tymed
	"Answer the receiver's tymed field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

tymed: anObject
	"Set the receiver's tymed field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!STGMEDIUM categoriesFor: #basicFree!private!realizing/unrealizing! !
!STGMEDIUM categoriesFor: #getValidFields!accessing!private! !
!STGMEDIUM categoriesFor: #hBitmap!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hBitmap:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hEnhMetaFile!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hEnhMetaFile:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hGlobal!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hGlobal:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hMetaFilePict!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #hMetaFilePict:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #lpszFileName!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #lpszFileName:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #medium!accessing!public! !
!STGMEDIUM categoriesFor: #mediums!accessing!public! !
!STGMEDIUM categoriesFor: #pstg!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #pstg:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #pstm!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #pstm:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #pUnkForRelease!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #pUnkForRelease:!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #tymed!**compiled accessors**!public! !
!STGMEDIUM categoriesFor: #tymed:!**compiled accessors**!public! !

!STGMEDIUM class methodsFor!

defineFields
	"Define the fields of the STGMEDIUM structure.
		STGMEDIUM compileDefinition
	
typedef struct tagSTGMEDIUM 
{ 
    DWORD tymed; 
    [switch_type(DWORD), switch_is((DWORD) tymed)] 
    union { 
        [case(TYMED_GDI)]      HBITMAP        hBitmap; 
        [case(TYMED_MFPICT)]   HMETAFILEPICT  hMetaFilePict; 
        [case(TYMED_ENHMF)]    HENHMETAFILE   hEnhMetaFile; 
        [case(TYMED_HGLOBAL)]  HGLOBAL        hGlobal; 
        [case(TYMED_FILE)]     LPWSTR         lpszFileName; 
        [case(TYMED_ISTREAM)]  IStream        *pstm; 
        [case(TYMED_ISTORAGE)] IStorage       *pstg; 
        [default] ; 
    }; 
    [unique] IUnknown *pUnkForRelease; 
}STGMEDIUM; 
typedef STGMEDIUM *LPSTGMEDIUM; 
 
	"

	self
		defineField: #tymed
			type: DWORDField new
			offset: 0;
		defineField: #hBitmap
			type: HANDLEField new
			offset: 4;
		defineField: #hMetaFilePict
			type: HANDLEField new
			offset: 4;
		defineField: #hEnhMetaFile
			type: HANDLEField new
			offset: 4;
		defineField: #hGlobal
			type: HANDLEField new
			offset: 4;
		defineField: #lpszFileName
			type: (PointerField type: UnicodeString)
			offset: 4;
		defineField: #pstm
			type: (PointerField type: IStream)
			offset: 4;
		defineField: #pstg
			type: (PointerField type: IStorage)
			offset: 4;
		defineField: #pUnkForRelease
			type: (PointerField type: IUnknown)
			offset: 8.
	self byteSize: 12! !
!STGMEDIUM class categoriesFor: #defineFields!initializing!public! !

