"Filed out from Dolphin Smalltalk 7"!

IUnknown subclass: #IDataObject
	instanceVariableNames: ''
	classVariableNames: 'RegisteredFormats'
	poolDictionaries: 'OLEShellDataTransferConstants'
	classInstanceVariableNames: ''!
IDataObject guid: (IID fromString: '{0000010E-0000-0000-C000-000000000046}')!
IDataObject comment: 'IDataObject is a wrapper class for the IDataObject COM interface.

Note: IDataObject makes data available via a number of formats and a number of mediums.

A full wrapping of IDataObject was outside the scope of this project.

We have only implemented helper/access methods that were needed for various parts of the Shell Project.
'!
!IDataObject categoriesForClass!Unclassified! !
!IDataObject methodsFor!

availableFormatEtcs
	^(self enumFormatEtc: 1) contents!

availableFormats
	| names name |
	^self availableFormatEtcs collect: 
			[:e | 
			name := Clipboard current getFormatName: e cfFormat.
			name isNil 
				ifTrue: 
					[names := OLEShellDataTransferConstants namesEqual: e cfFormat prefix: 'CF_'.
					names notEmpty ifTrue: [name := names first]].
			name]!

enumFormatEtc: dwDirection
	"dwDirection 
		[in] Direction of the data through a value from the enumeration DATADIR 
			DATADIR_GET = 1 
			DATADIR_SET = 2"

	| answer |
	answer := IEnumFORMATETC newPointer.
	self EnumFormatEtc: dwDirection ppenumFormatEtc: answer.
	^answer!

EnumFormatEtc: enumFormatEtc ppenumFormatEtc: ppenumFormatEtc
	"Callout for the IDataObject::EnumFormatEtc() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDataObject>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 9 dword lppvoid>
	^self invalidCall!

formatEtc: formatName ifNone: operation
	| formatValue |
	formatValue := self class formatValueFor: formatName.
	^self availableFormatEtcs detect: [:each | each cfFormat = formatValue] ifNone: operation!

GetCanonicalFormatEtc: getCanonicalFormatEtc pformatetcOut: pformatetcOut
	"Callout for the IDataObject::GetCanonicalFormatEtc() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDataObject>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 7 FORMATETC* FORMATETC*>
	^self invalidCall!

GetData: getData pmedium: pmedium
	"Callout for the IDataObject::GetData() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDataObject>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 4 FORMATETC* STGMEDIUM*>
	^self invalidCall!

GetDataHere: getDataHere pmedium: pmedium
	"Callout for the IDataObject::GetDataHere() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDataObject>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 5 FORMATETC* STGMEDIUM*>
	^self invalidCall!

getDataMedium: aFormatEtc
	| medium |
	self GetData: aFormatEtc pmedium: (medium := STGMEDIUM new).
	^medium!

getDropFilesIfNone: operation
	| formatEtc medium |
	formatEtc := self formatEtc: CF_HDROP ifNone: [^operation value].
	self assert: [formatEtc tymed anyMask: TYMED_HGLOBAL].
	"medium is an object that we should be freeing using finalization"
	medium := self getDataMedium: formatEtc.
	self assert: [medium tymed anyMask: TYMED_HGLOBAL].
	^ShellLibrary default dragQueryFile: medium hGlobal!

getFileContentsAt: anIndex
	"SW: Not tested ... I couldnt get the shell to produce this format"

	| formatEtc medium |
	formatEtc := (FORMATETC new)
				cfFormat: (self class formatValueFor: 'FileContents');
				lindex: anIndex - 1;
				yourself.
	medium := STGMEDIUM new.
	medium tymed: TYMED_ISTREAM.
	self GetData: formatEtc pmedium: medium.
	self assert: [medium tymed anyMask: TYMED_ISTREAM].
	^medium pstm!

getFileContentsIfNone: operation
	"SW: Not tested ... I couldnt get the shell to produce this format"

	| formatEtc medium hglobal descriptors answer |
	formatEtc := self formatEtc: 'FileGroupDescriptor' ifNone: [^operation value].
	self assert: [formatEtc tymed anyMask: TYMED_HGLOBAL].
	"medium is an object that we should be freeing using finalization"
	medium := self getDataMedium: formatEtc.
	"Extract the CFSTR_FILEDESCRIPTOR format as a TYMED_HGLOBAL value"
	self assert: [medium tymed anyMask: TYMED_HGLOBAL].
	"The hGlobal member of the returned STGMEDIUM structure points to a global memory object. 
	Lock that object by passing the hGlobal value to GlobalLock. "
	hglobal := KernelLibrary default globalLock: medium hGlobal.
	"Cast the pointer returned by GlobalLock to a FILEGROUPDESCRIPTOR  pointer. It will point to a FILEGROUPDESCRIPTOR structure followed by one or more FILEDESCRIPTOR  structures. Each FILEDESCRIPTOR structure contains a description of a file that is contained by one of the accompanying CFSTR_FILECONTENTS formats. "
	
	[descriptors := (FILEGROUPDESCRIPTOR fromAddress: hglobal) copy fgd.
	answer := OrderedCollection new.
	descriptors 
		keysAndValuesDo: [:i :descriptor | answer add: (descriptor -> self getFileContentsAt: i)]] 
			ensure: [KernelLibrary default globalUnlock: hglobal].
	^answer!

getText
	"Answer a <readableString> that is the receiver's contents, or nil."

	| fmt stgm hr pText text |
	(self isFormatAvailable: CF_TEXT) ifFalse: [^nil].
	fmt := (FORMATETC new)
				cfFormat: CF_TEXT;
				dwAspect: DVASPECT_CONTENT;
				lindex: -1;
				tymed: TYMED_HGLOBAL.
	stgm := STGMEDIUM new.
	(hr := self GetData: fmt pmedium: stgm) = S_OK ifFalse: [^nil].
	pText := KernelLibrary default globalLock: stgm hGlobal.
	text := String fromAddress: pText.
	KernelLibrary default globalUnlock: stgm hGlobal.
	^text!

getTextAsStream
	"Answer a <readableString> that is the receiver's contents, or nil."

	| fmt stgm hr |
	(self isFormatAvailable: CF_TEXT) ifFalse: [^nil].
	fmt := (FORMATETC new)
				cfFormat: CF_TEXT;
				dwAspect: DVASPECT_CONTENT;
				lindex: -1;
				tymed: TYMED_ISTREAM.
	stgm := STGMEDIUM new.
	(hr := self GetData: fmt pmedium: stgm) = S_OK ifFalse: [^nil].
	^stgm pstm contents asString trimNulls!

isFormatAvailable: format
	"Answer whether the clipboard contains data in the specified
	<integer> format (e.g. CF_TEXT)."

	| formatValue |
	formatValue := self class formatValueFor: format.
	^self availableFormatEtcs anySatisfy: [:each | each cfFormat = formatValue]!

isObjectAvailable
	"Answer whether the receiver has #Object amongst its available formats."

	^self isFormatAvailable: #Object!

QueryGetData: queryGetData
	"Callout for the IDataObject::QueryGetData() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDataObject>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 6 FORMATETC*>
	^self invalidCall!

SetData: setData pmedium: pmedium fRelease: fRelease
	"Callout for the IDataObject::SetData() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDataObject>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 8 FORMATETC* STGMEDIUM* bool>
	^self invalidCall! !
!IDataObject categoriesFor: #availableFormatEtcs!accessing!public! !
!IDataObject categoriesFor: #availableFormats!accessing!public! !
!IDataObject categoriesFor: #enumFormatEtc:!accessing!public! !
!IDataObject categoriesFor: #EnumFormatEtc:ppenumFormatEtc:!COM Interfaces-IDataObject!public! !
!IDataObject categoriesFor: #formatEtc:ifNone:!accessing!public! !
!IDataObject categoriesFor: #GetCanonicalFormatEtc:pformatetcOut:!COM Interfaces-IDataObject!public! !
!IDataObject categoriesFor: #GetData:pmedium:!COM Interfaces-IDataObject!public! !
!IDataObject categoriesFor: #GetDataHere:pmedium:!COM Interfaces-IDataObject!public! !
!IDataObject categoriesFor: #getDataMedium:!accessing!public! !
!IDataObject categoriesFor: #getDropFilesIfNone:!accessing!public! !
!IDataObject categoriesFor: #getFileContentsAt:!accessing!public! !
!IDataObject categoriesFor: #getFileContentsIfNone:!accessing!public! !
!IDataObject categoriesFor: #getText!accessing!public! !
!IDataObject categoriesFor: #getTextAsStream!accessing!public! !
!IDataObject categoriesFor: #isFormatAvailable:!public!testing! !
!IDataObject categoriesFor: #isObjectAvailable!public!testing! !
!IDataObject categoriesFor: #QueryGetData:!COM Interfaces-IDataObject!public! !
!IDataObject categoriesFor: #SetData:pmedium:fRelease:!COM Interfaces-IDataObject!public! !

!IDataObject class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'OAIDL.IDataObject'
		IDataObject  compileFunctions
		IDataObject  compileDefinition
		IDataObject  defineTemplate
	"

	self
		defineFunction: #GetData:pmedium: argumentTypes: 'FORMATETC* STGMEDIUM*';
		defineFunction: #GetDataHere:pmedium: argumentTypes: 'FORMATETC* STGMEDIUM*';
		defineFunction: #QueryGetData: argumentTypes: 'FORMATETC*';
		defineFunction: #GetCanonicalFormatEtc:pformatetcOut:
			argumentTypes: 'FORMATETC* FORMATETC*';
		defineFunction: #SetData:pmedium:fRelease: argumentTypes: 'FORMATETC* STGMEDIUM* bool';
		defineFunction: #EnumFormatEtc:ppenumFormatEtc: argumentTypes: 'dword lppvoid'!

exampleClipboard
	"Use Case 1:  Extracting the File Names from the Data Object"

	"Retrieve the IDataObject interface"

	| dataObject |
	dataObject := IDataObject oleClipboard.
	"Enumerate and answer allAvailableFormatEtcs"
	dataObject availableFormatEtcs.
	"Use the CF_HDROP format to retrieve fileNames"

	"Copy some text from here ... should answer 'none'"
	IDataObject oleClipboard getDropFilesIfNone: ['none'].
	"In Windows Explorer, select a file, right-click, and select copy"
	IDataObject oleClipboard getDropFilesIfNone: ['none'].
	"In Windows Explorer, select multiple files, right-click, and select copy"
	IDataObject oleClipboard getDropFilesIfNone: ['none']!

formatValueFor: formatName
	"MSDN:  Shell format identifiers have the form CFSTR_XXX and are not predefined. For simplicity, the CFSTR_XXX values are often referred to as formats. However, unlike predefined formats, they must be registered by both source and target before they can be used to transfer data. To register a Shell format, include the Shlobj.h header file and pass the CFSTR_XXX format identifier to RegisterClipBoardFormat. This function returns a valid clipboard format value, which can then be used as the cfFormat member of a FORMATETC structure."

	(formatName isKindOf: Integer) ifTrue: [^formatName].
	^RegisteredFormats at: formatName
		ifAbsentPut: [UserLibrary default registerClipboardFormat: formatName asParameter]!

initialize
	"
		self initialize
	"

	super initialize.
	RegisteredFormats := LookupTable new.
	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self!

oleClipboard
	"Answer an instance of the receiver representing what's on the clipboard."

	| instance |
	instance := self newPointer.
	OLELibrary default oleGetClipboard: instance.
	^instance!

onStartup
	RegisteredFormats := LookupTable new!

uninitialize
	SessionManager current removeEventsTriggeredFor: self! !
!IDataObject class categoriesFor: #defineFunctions!initializing!public! !
!IDataObject class categoriesFor: #exampleClipboard!examples!public! !
!IDataObject class categoriesFor: #formatValueFor:!instance creation!public! !
!IDataObject class categoriesFor: #initialize!initializing!public! !
!IDataObject class categoriesFor: #oleClipboard!instance creation!public! !
!IDataObject class categoriesFor: #onStartup!event handling!instance creation!public! !
!IDataObject class categoriesFor: #uninitialize!initializing!public! !

