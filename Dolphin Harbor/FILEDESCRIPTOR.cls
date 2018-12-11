"Filed out from Dolphin Smalltalk 7"!

OLEStructure subclass: #FILEDESCRIPTOR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FILEDESCRIPTOR guid: (GUID fromString: '{04e70b60-6256-4473-9451-8c2d02c0a695}')!
FILEDESCRIPTOR comment: 'FILEDESCRIPTOR is an <ExternalStructure> that wraps the Windows FILEDESCRIPTOR structure.  An instance of FILEDESCRIPTOR describes the properties of a file that is being copied by means of the clipboard during an OLE drag-and-drop operation. 

[Edit the following which is almost verbatim from MSDN]

- dwFlags, an array of flags that indicate which of the other structure members contain valid data. This member can be a combination of the following values: FD_ACCESSTIME, FD_ATTRIBUTES, FD_CLSID, FD_CREATETIME, FD_FILESIZE, FD_LINKU, FD_SIZEPOINT, and FD_WRITESTIME.
- clsid, File class identifier. 
- sizel, Width and height of the file icon. 
- pointl, Screen coordinates of the file object. 
- dwFileAttributes, File attribute flags. This will be a combination of the FILE_ATTRIBUTE_ values described in GetFileAttributes. 
- ftCreationTime, FILETIME structure that contains the time of file creation. 
- ftLastAccessTime, FILETIME structure that contains the time that the file was last accessed. 
- ftLastWriteTime, FILETIME structure that contains the time of the last write operation. 
- nFileSizeHigh, High-order DWORD of the file size, in bytes. 
- nFileSizeLow, Low-order DWORD of the file size, in bytes. 
- cFileName, Null-terminated string that contains the name of the file. 

Remarks
If the CFSTR_FILECONTENTS format that corresponds to this structure contains the file as a global memory object, nFileSizeHigh and nFileSizeLow specify the size of the associated memory block. If they are set, they can also be used if a user-interface needs to be displayed. For instance, if a file is about to be overwritten, you would normally use information from this structure to display a dialog box containing the size, data, and name of the file.

To create a zero-length file, set the FD_FILESIZE flag in the dwFlags member, and set nFileSizeHigh and nFileSizeLow to zero. The CFSTR_FILECONTENTS format should represent the file as either a stream or global memory object (TYMED_ISTREAM or TYMED_HGLOBAL).'!
!FILEDESCRIPTOR categoriesForClass!Unclassified! !
!FILEDESCRIPTOR methodsFor!

cFileName
	"Answer the receiver's cFileName field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 72)!

clsid
	"Answer the receiver's clsid field as a Smalltalk object."

	^GUID fromAddress: (bytes yourAddress + 4)!

clsid: anObject
	"Set the receiver's clsid field to the value of anObject."

	anObject replaceBytesOf: bytes from: 5 to: 20 startingAt: 1!

dwFileAttributes
	"Answer the receiver's dwFileAttributes field as a Smalltalk object."

	^(bytes dwordAtOffset: 36)!

dwFileAttributes: anObject
	"Set the receiver's dwFileAttributes field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

dwFlags
	"Answer the receiver's dwFlags field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

dwFlags: anObject
	"Set the receiver's dwFlags field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

ftCreationTime
	"Answer the receiver's ftCreationTime field as a Smalltalk object."

	^FILETIME fromAddress: (bytes yourAddress + 40)!

ftCreationTime: anObject
	"Set the receiver's ftCreationTime field to the value of anObject."

	anObject replaceBytesOf: bytes from: 41 to: 48 startingAt: 1!

ftLastAccessTime
	"Answer the receiver's ftLastAccessTime field as a Smalltalk object."

	^FILETIME fromAddress: (bytes yourAddress + 48)!

ftLastAccessTime: anObject
	"Set the receiver's ftLastAccessTime field to the value of anObject."

	anObject replaceBytesOf: bytes from: 49 to: 56 startingAt: 1!

ftLastWriteTime
	"Answer the receiver's ftLastWriteTime field as a Smalltalk object."

	^FILETIME fromAddress: (bytes yourAddress + 56)!

ftLastWriteTime: anObject
	"Set the receiver's ftLastWriteTime field to the value of anObject."

	anObject replaceBytesOf: bytes from: 57 to: 64 startingAt: 1!

nFileSizeHigh
	"Answer the receiver's nFileSizeHigh field as a Smalltalk object."

	^(bytes dwordAtOffset: 68)!

nFileSizeHigh: anObject
	"Set the receiver's nFileSizeHigh field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

pointl
	"Answer the receiver's pointl field as a Smalltalk object."

	^POINTL fromAddress: (bytes yourAddress + 28)!

pointl: anObject
	"Set the receiver's pointl field to the value of anObject."

	anObject replaceBytesOf: bytes from: 29 to: 36 startingAt: 1!

sizel
	"Answer the receiver's sizel field as a Smalltalk object."

	^SIZE fromAddress: (bytes yourAddress + 20)!

sizel: anObject
	"Set the receiver's sizel field to the value of anObject."

	anObject replaceBytesOf: bytes from: 21 to: 28 startingAt: 1! !
!FILEDESCRIPTOR categoriesFor: #cFileName!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #clsid!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #clsid:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #dwFileAttributes!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #dwFileAttributes:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #dwFlags!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #dwFlags:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #ftCreationTime!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #ftCreationTime:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #ftLastAccessTime!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #ftLastAccessTime:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #ftLastWriteTime!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #ftLastWriteTime:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #nFileSizeHigh!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #nFileSizeHigh:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #pointl!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #pointl:!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #sizel!**compiled accessors**!public! !
!FILEDESCRIPTOR categoriesFor: #sizel:!**compiled accessors**!public! !

!FILEDESCRIPTOR class methodsFor!

defineFields
	"Define the Win32 FILEDESCRIPTOR structure
		FILEDESCRIPTOR compileDefinition

	typedef struct _FILEDESCRIPTOR { 
	    DWORD    dwFlags; 
	    CLSID    clsid; 
	    SIZEL    sizel; 
	    POINTL   pointl; 
	    DWORD    dwFileAttributes; 
	    FILETIME ftCreationTime; 
	    FILETIME ftLastAccessTime; 
	    FILETIME ftLastWriteTime; 
	    DWORD    nFileSizeHigh; 
	    DWORD    nFileSizeLow; 
	    TCHAR    cFileName[MAX_PATH]; 
	} FILEDESCRIPTOR, *LPFILEDESCRIPTOR; 

	"

	self
		defineField: #dwFlags type: DWORDField new;
		defineField: #clsid type: (StructureField type: GUID);
		defineField: #sizel type: (StructureField type: SIZE);
		defineField: #pointl type: (StructureField type: POINTL);
		defineField: #dwFileAttributes type: DWORDField new;
		defineField: #ftCreationTime type: (StructureField type: FILETIME);
		defineField: #ftLastAccessTime type: (StructureField type: FILETIME);
		defineField: #ftLastWriteTime type: (StructureField type: FILETIME);
		defineField: #nFileSizeHigh type: DWORDField new;
		defineField: #nFileSizeHigh type: DWORDField new;
		defineField: #cFileName type: (StringField length: File maxPath) beReadOnly! !
!FILEDESCRIPTOR class categoriesFor: #defineFields!initializing!public! !

