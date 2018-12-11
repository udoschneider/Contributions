| package |
package := Package name: 'DH Shell Data Transfer'.
package paxVersion: 1;
	basicComment: 'Shell Data Transfer

Copyright (c) Louis Sumberg and Steve Waring 2002.
	<lsumberg@mindspring.com>, <http://www.mindspring.com/~lsumberg/dolphin>
	<swaring@ozemail.com.au>, <http://www.dolphinharbor.org>
Public Domain Freeware.

This package adds the capability for data to be transferred via the OLE clipboard and through OLE drag and drop.  This also allows for data transfer between Dolphin and non-Dolphin applications.

The key classes in this package are ShellDragDropSession and IDataObject.  In general, a View registers itself as a potential drop target with ShellDragDropSession, which creates an instance of itself and handles drag and drop events.  This instance also interacts with IDataObject to extract the data that is being transferred.  Other classes in the package, such as FORMATETC and STGMEDIUM, represent Windows structures and are used in Windows API calls.
'.

package basicPackageVersion: '5.1.2.5'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIAAAAAA=').

package classNames
	add: #DROPFILES;
	add: #FILEDESCRIPTOR;
	add: #FILEGROUPDESCRIPTOR;
	add: #FORMATETC;
	add: #IDataObject;
	add: #IDropTarget;
	add: #IEnumFORMATETC;
	add: #ShellDragDropSample;
	add: #ShellDragDropSession;
	add: #STGMEDIUM;
	yourself.

package methodNames
	add: #OLELibrary -> #oleGetClipboard:;
	add: #OLELibrary -> #registerDragDrop:pDropTarget:;
	add: #OLELibrary -> #releaseStgMedium:;
	add: #OLELibrary -> #revokeDragDrop:;
	add: #ShellLibrary -> #dragQueryFile:;
	yourself.

package globalNames
	add: #OLEShellDataTransferConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: '..\Object Arts\Dolphin\ActiveX\Structured Storage\OLE Structured Storage';
	add: '..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

DragDropSession subclass: #ShellDragDropSession
	instanceVariableNames: 'keyState dataObject operationDescriptions registeredView'
	classVariableNames: 'Register'
	poolDictionaries: 'OLEConstants OLEShellDataTransferConstants Win32Errors'
	classInstanceVariableNames: ''!
IUnknown subclass: #IDataObject
	instanceVariableNames: ''
	classVariableNames: 'RegisteredFormats'
	poolDictionaries: 'OLEShellDataTransferConstants'
	classInstanceVariableNames: ''!
IUnknown subclass: #IDropTarget
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IEnumXXXX subclass: #IEnumFORMATETC
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OLEStructure subclass: #DROPFILES
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OLEStructure subclass: #FILEDESCRIPTOR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OLEStructure subclass: #FILEGROUPDESCRIPTOR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OLEStructure subclass: #FORMATETC
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OLEFinalizableStructure subclass: #STGMEDIUM
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'OLEShellDataTransferConstants'
	classInstanceVariableNames: ''!
Shell subclass: #ShellDragDropSample
	instanceVariableNames: 'textPresenter fullTextPresenter listPresenter listBoxPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!OLELibrary methodsFor!

oleGetClipboard: ppDataObj
	"Retrieves a data object that you can use to access the contents of the clipboard.


		WINOLEAPI OleGetClipboard(
			  IDataObject ** ppDataObj  //Address of output variable that  receives the IDataObject interface 
	);"

	<stdcall: hresult OleGetClipboard lppvoid>
	^self invalidCall!

registerDragDrop: hwnd pDropTarget: anIDropTarget
	"Retrieves a data object that you can use to access the contents of the clipboard.

	WINOLEAPI RegisterDragDrop(
		HWND hwnd, //Handle to a window that can accept drops
		IDropTarget * pDropTarget	//Pointer to object that is to be target of drop
	); "

	<stdcall: hresult RegisterDragDrop handle IDropTarget*>
	^self invalidCall!

releaseStgMedium: aSTGMEDIUM
	"Frees the specified storage medium..

	void ReleaseStgMedium(
  	  STGMEDIUM * pmedium  //Pointer to storage medium to be freed
	);"

	<stdcall: void ReleaseStgMedium STGMEDIUM*>
	^self invalidCall!

revokeDragDrop: hwnd
	"Revokes the registration of the specified application window as a potential target for OLE drag-and-drop operations.

	WINOLEAPI RevokeDragDrop(
		HWND hwnd //Handle to a window that can accept drops
	); "

	<stdcall: hresult RevokeDragDrop handle>
	^self invalidCall! !
!OLELibrary categoriesFor: #oleGetClipboard:!OLE Functions-Structured Storage!primitives!public! !
!OLELibrary categoriesFor: #registerDragDrop:pDropTarget:!OLE Functions-Structured Storage!primitives!public! !
!OLELibrary categoriesFor: #releaseStgMedium:!OLE Functions-Structured Storage!primitives!public! !
!OLELibrary categoriesFor: #revokeDragDrop:!OLE Functions-Structured Storage!primitives!public! !

!ShellLibrary methodsFor!

dragQueryFile: hDrop
	"Answer a collection of <String>s"

	| num answer buffer |
	#shellAdded.
	answer := OrderedCollection new.
	(num := self 
				dragQueryFile: hDrop
				iFile: 4294967295
				lpszFile: nil
				cch: 0) = 4294967295 
		ifTrue: [^answer].
	0 to: num - 1
		do: 
			[:i | 
			buffer := File pathBuffer.
			self 
				dragQueryFile: hDrop
				iFile: i
				lpszFile: buffer
				cch: buffer size.
			answer add: buffer trimNulls].
	^answer! !
!ShellLibrary categoriesFor: #dragQueryFile:!public!win32 functions-shell library! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #OLEShellDataTransferConstants put: (PoolConstantsDictionary named: #OLEShellDataTransferConstants)!
OLEShellDataTransferConstants at: 'CF_BITMAP' put: 16r2!
OLEShellDataTransferConstants at: 'CF_DIB' put: 16r8!
OLEShellDataTransferConstants at: 'CF_DIBV5' put: 16r11!
OLEShellDataTransferConstants at: 'CF_DIF' put: 16r5!
OLEShellDataTransferConstants at: 'CF_DSPBITMAP' put: 16r82!
OLEShellDataTransferConstants at: 'CF_DSPENHMETAFILE' put: 16r8E!
OLEShellDataTransferConstants at: 'CF_DSPMETAFILEPICT' put: 16r83!
OLEShellDataTransferConstants at: 'CF_DSPTEXT' put: 16r81!
OLEShellDataTransferConstants at: 'CF_ENHMETAFILE' put: 16rE!
OLEShellDataTransferConstants at: 'CF_GDIOBJFIRST' put: 16r300!
OLEShellDataTransferConstants at: 'CF_GDIOBJLAST' put: 16r3FF!
OLEShellDataTransferConstants at: 'CF_HDROP' put: 16rF!
OLEShellDataTransferConstants at: 'CF_LOCALE' put: 16r10!
OLEShellDataTransferConstants at: 'CF_MAX' put: 16r12!
OLEShellDataTransferConstants at: 'CF_METAFILEPICT' put: 16r3!
OLEShellDataTransferConstants at: 'CF_OEMTEXT' put: 16r7!
OLEShellDataTransferConstants at: 'CF_OWNERDISPLAY' put: 16r80!
OLEShellDataTransferConstants at: 'CF_PALETTE' put: 16r9!
OLEShellDataTransferConstants at: 'CF_PENDATA' put: 16rA!
OLEShellDataTransferConstants at: 'CF_PRIVATEFIRST' put: 16r200!
OLEShellDataTransferConstants at: 'CF_PRIVATELAST' put: 16r2FF!
OLEShellDataTransferConstants at: 'CF_RIFF' put: 16rB!
OLEShellDataTransferConstants at: 'CF_SYLK' put: 16r4!
OLEShellDataTransferConstants at: 'CF_TEXT' put: 16r1!
OLEShellDataTransferConstants at: 'CF_TIFF' put: 16r6!
OLEShellDataTransferConstants at: 'CF_UNICODETEXT' put: 16rD!
OLEShellDataTransferConstants at: 'CF_WAVE' put: 16rC!
OLEShellDataTransferConstants at: 'DVASPECT_CONTENT' put: 16r1!
OLEShellDataTransferConstants at: 'DVASPECT_COPY' put: 16r3!
OLEShellDataTransferConstants at: 'DVASPECT_DOCPRINT' put: 16r8!
OLEShellDataTransferConstants at: 'DVASPECT_ICON' put: 16r4!
OLEShellDataTransferConstants at: 'DVASPECT_LINK' put: 16r4!
OLEShellDataTransferConstants at: 'DVASPECT_SHORTNAME' put: 16r2!
OLEShellDataTransferConstants at: 'DVASPECT_THUMBNAIL' put: 16r2!
OLEShellDataTransferConstants at: 'TYMED_ENHMF' put: 16r40!
OLEShellDataTransferConstants at: 'TYMED_FILE' put: 16r2!
OLEShellDataTransferConstants at: 'TYMED_GDI' put: 16r10!
OLEShellDataTransferConstants at: 'TYMED_HGLOBAL' put: 16r1!
OLEShellDataTransferConstants at: 'TYMED_ISTORAGE' put: 16r8!
OLEShellDataTransferConstants at: 'TYMED_ISTREAM' put: 16r4!
OLEShellDataTransferConstants at: 'TYMED_MFPICT' put: 16r20!
OLEShellDataTransferConstants at: 'TYMED_NULL' put: 16r0!
OLEShellDataTransferConstants shrink!

"Classes"!

ShellDragDropSession guid: (GUID fromString: '{93E57526-C0BE-48A8-9881-18746A7174D6}')!
ShellDragDropSession comment: 'This class attempts to integrate WindowsShell drag/drop into Dolphin''s drag/drop framework.

An instance is created and registered for each View that wishes to receive drap/drop from the WindowsShell. 

Instances implement the IDropTarget interface. The functions of this interface are implemented by converting the arguments into objects and then acting like a DragDropSession.

Note: Only a subset of formats/mediums are made available to presenters. See the method #createDragObjects

Instance Variables
	keyState			<Integer>
	dataObject		<IDataObject>
	operationDescriptions	<Dictionary>
	registeredView		<View>'!
!ShellDragDropSession categoriesForClass!Unclassified! !
!ShellDragDropSession methodsFor!

continueTrackingAt: aPoint from: lastPoint
	"Move the representation of the DragDropObject from lastPoint to aPoint."

	| newDropTarget |
	dragPoint := aPoint.
	(newDropTarget := View fromPoint: dragPoint) = dropTarget
		ifTrue: [self dropTargetOver]
		ifFalse: [
			self dropTargetLeave.
			dropTarget := newDropTarget.
			self dropTargetEnter].
	self giveFeedback.
	^dragPoint!

createDragObjects
	"Answer a Collection containing a DragDropObject.
	Note: IDataObject makes data available via a number of formats and a number of mediums.
		- A full wrapping of IDataObject was outside the scope of this project.
		- We make the filenames available and the IDataObject if the user needs fuller access"

	| fileNames dragObject |
	fileNames := dataObject getDropFilesIfNone: [#()].
	dragObjects := OrderedCollection with: (dragObject := self newDragObject: fileNames).
	dragObject format: #Filenames data: fileNames.
	dragObject format: #IDataObject data: dataObject.
	fileNames notEmpty ifTrue: [dragObject format: #String data: fileNames first]!

dragButton
	(self keyState anyMask: MK_RBUTTON) ifTrue: [^#right].
	^#left!

DragEnter: anIDataObject grfKeyState: grfKeyState pt: pt pdwEffect: aDWORD
	"<virtual stdcall: hresult 4 IDataObject* dword POINTL DWORD*>
	^self invalidCall"

	"Notification signal: anIDataObject availableFormatEtcs."

	keyState := grfKeyState.
	dataObject := anIDataObject.
	dragPoint := pt asPoint.
	dropTarget := registeredView.
	self createDragObjects.
	self dropTargetEnter.
	aDWORD value: self dropEffect.
	^S_OK!

DragLeave
	"<virtual stdcall: hresult 6>
	^self invalidCall"

	keyState := dataObject := dragObjects := nil.
	^S_OK!

dragObjectClass
	^InternalDragDropObject!

DragOver: dragOver pt: pt pdwEffect: aDWORD
	"<virtual stdcall: hresult 5 dword POINTL DWORD*>
	^self invalidCall"

	self continueTrackingAt: (dragPoint := pt asPoint) from: nil.
	aDWORD value: self dropEffect.
	^S_OK!

Drop: anIDataObject grfKeyState: grfKeyState pt: pt pdwEffect: aDWORD
	"<virtual stdcall: hresult 7 IDataObject* dword POINTL DWORD*>
	^self invalidCall"

	dataObject := anIDataObject.
	self createDragObjects.
	self continueTrackingAt: (dragPoint := pt asPoint) from: nil.
	operation isNil 
		ifTrue: [self dropTargetLeave]
		ifFalse: 
			[(self dropTargetDrop and: [operation == #move]) 
				ifTrue: 
					["self dragSourceCut"

					]].
	keyState := dataObject := dragObjects := nil.
	aDWORD value: self dropEffect.
	^S_OK!

dropEffect
	| map |
	(map := IdentityDictionary new)
		at: #copy put: 1;
		at: #move put: 2;
		at: #link put: 4.
	^map at: self operation ifAbsent: [0]!

finalRelease
	!

forcedOperation
	"If the user is currently holding down one of the drag and drop modifier keys
	answer the operation intended. If no such keys are held down then answer nil."

	(self keyState anyMask: MK_CONTROL) ifTrue: [^#copy].
	(self keyState anyMask: MK_SHIFT) ifTrue: [^#move].
	"self keyState anyMask: MK_ALT ifTrue: [^#link]."
	^nil!

getExtendedOperation
	"Private - An extended drop has occurred - query the drop target for a list of supported operations
	so that we may present these as a context menu to allow the user to choose the operation."

	| opNames supported |
	self dropTarget isDropTarget ifTrue: [dropTarget ddOperations: self].
	(supported := self supportedOperations) isEmpty 
		ifTrue: [self operation: nil]
		ifFalse: 
			[| popup defaultOp i default |
			defaultOp := self operation.
			popup := Menu new.
			i := 1.
			default := 0.
			opNames := self operationDescriptions.
			supported do: 
					[:opSymbol | 
					opSymbol == defaultOp ifTrue: [default := i].
					popup addCommand: (MessageSend 
								receiver: self
								selector: #operation:
								argument: opSymbol)
						description: (opNames at: opSymbol).
					i := i + 1].
			popup
				addSeparator;
				addCommand: (MessageSend 
							receiver: self
							selector: #operation:
							argument: nil)
					description: 'Cancel'.
			popup setDefault: default.
			self operation: nil.
			popup showIn: self dropTarget position: Cursor position.
			"Allow the command selected from the menu to be dispatched before returning"
			SessionManager inputState pumpMessages].
	^operation!

giveFeedback
	"Private - Set the drag cursor to indicate the effect of a drop on the current target."

	!

hideDragImage
	"Do Nothing"

!

intendedDropEffect
	| map |
	(map := IdentityDictionary new)
		at: #copy put: 1;
		at: #move put: 2;
		at: #link put: 4.
	^map at: self intendedOperation ifAbsent: [0]!

isExtendedDrag
	"Answer whether this drag was initiated with the 'right' mouse button."

	^self dragButton == self class extendedDragButton!

keyState
	^keyState!

operationDescriptions
	"Answer an <abstractDictionary> that maps the standard drag and drop operations symbols
	to menu item names for the purposes of an extended drag and drop (i.e. these are used as the
	descriptions on the right drag popup menu)."

	operationDescriptions isNil ifTrue: [operationDescriptions := super operationDescriptions].
	^operationDescriptions!

queryInterface: anInterfaceClass
	"Answer a new interface which supports the specified interface protocol
	(usually a class), or nil if the receiver does not support the interface."

	^self queryInterface: anInterfaceClass ifNone: []
!

queryInterface: anInterfaceClass ifNone: exceptionHandler
	"Answer a new interface pointer which supports the specified interface protocol
	(usually a class). If the receiver does not support the interface, answer the
	result of evaluating the niladic valuable, exceptionHandler."

	(IDropTarget supportsInterface: anInterfaceClass) ifFalse: [^exceptionHandler value].
	^IDropTarget on: self implementor: self!

registeredView
	^registeredView!

registeredView: anObject
	registeredView := anObject!

revokeDragDrop
	OLELibrary default revokeDragDrop: registeredView handle!

server
	"Answer the server object on behalf of which the receiver is implementing an
	interface or interfaces. By default this is the receiver itself, but subclasses
	may implement interfaces on behalf of other objects in order that any particular
	server object can support any number of interfaces with different implementors."

	^self!

showDragImage
	"Do Nothing"

	!

solidifyOperation
	"Private - Time to make up our minds on the actual drag operation to be performed.
	By default we assume it has already solidified."

	self isExtendedDrag ifTrue: [
		self getExtendedOperation]!

startTrackingAt: aPoint
	Error shouldNotImplement! !
!ShellDragDropSession categoriesFor: #continueTrackingAt:from:!public!tracking! !
!ShellDragDropSession categoriesFor: #createDragObjects!operations!public! !
!ShellDragDropSession categoriesFor: #dragButton!accessing!public! !
!ShellDragDropSession categoriesFor: #DragEnter:grfKeyState:pt:pdwEffect:!COM Interfaces-IDropTarget!public! !
!ShellDragDropSession categoriesFor: #DragLeave!COM Interfaces-IDropTarget!public! !
!ShellDragDropSession categoriesFor: #dragObjectClass!constants!public! !
!ShellDragDropSession categoriesFor: #DragOver:pt:pdwEffect:!COM Interfaces-IDropTarget!public! !
!ShellDragDropSession categoriesFor: #Drop:grfKeyState:pt:pdwEffect:!COM Interfaces-IDropTarget!public! !
!ShellDragDropSession categoriesFor: #dropEffect!accessing!public! !
!ShellDragDropSession categoriesFor: #finalRelease!initializing!public! !
!ShellDragDropSession categoriesFor: #forcedOperation!accessing!public! !
!ShellDragDropSession categoriesFor: #getExtendedOperation!operations!private! !
!ShellDragDropSession categoriesFor: #giveFeedback!operations!private! !
!ShellDragDropSession categoriesFor: #hideDragImage!operations!public! !
!ShellDragDropSession categoriesFor: #intendedDropEffect!accessing!public! !
!ShellDragDropSession categoriesFor: #isExtendedDrag!public!testing! !
!ShellDragDropSession categoriesFor: #keyState!accessing!public! !
!ShellDragDropSession categoriesFor: #operationDescriptions!accessing!public! !
!ShellDragDropSession categoriesFor: #queryInterface:!accessing!public! !
!ShellDragDropSession categoriesFor: #queryInterface:ifNone:!accessing!public! !
!ShellDragDropSession categoriesFor: #registeredView!accessing!private! !
!ShellDragDropSession categoriesFor: #registeredView:!accessing!private! !
!ShellDragDropSession categoriesFor: #revokeDragDrop!initializing!public! !
!ShellDragDropSession categoriesFor: #server!accessing!public! !
!ShellDragDropSession categoriesFor: #showDragImage!operations!public! !
!ShellDragDropSession categoriesFor: #solidifyOperation!operations!private! !
!ShellDragDropSession categoriesFor: #startTrackingAt:!public!tracking! !

!ShellDragDropSession class methodsFor!

initialize
	"
		self initialize
	"

	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self.
	SessionManager current 
		when: #sessionStopped
		send: #onExit
		to: self.
	self initializeRegister!

initializeRegister
	Register := WeakIdentityDictionary new!

onExit
	Register := nil!

onStartup
	"Private - The image has just started. Queue a deferredAction to re-register all drop targets once the View and COM systems
	have all be started up"

	SessionManager inputState queueDeferredAction: [self reregisterAll]!

registerDropTarget: aView
	"Answer and register an instance which is registered to receive shell drag/drop method sends for aView"

	| instance interface |
	instance := self new registeredView: aView.
	interface := instance queryInterface: IDropTarget.
	OLELibrary default registerDragDrop: aView handle pDropTarget: interface.
	Register at: aView put: instance.
	^instance!

reregisterAll
	| oldViews |
	oldViews := Register keys.
	self initializeRegister.
	oldViews do: [:each | each isOpen ifTrue: [self registerDropTarget: each]]!

revokeDropTarget: aView
	"Answer an instance which is registered to receive shell drag/drop method sends for aView"

	(Register removeKey: aView ifAbsent: [^nil]) revokeDragDrop!

uninitialize
	SessionManager current removeEventsTriggeredFor: self.
	Register := nil! !
!ShellDragDropSession class categoriesFor: #initialize!instance creation!public! !
!ShellDragDropSession class categoriesFor: #initializeRegister!initializing!public! !
!ShellDragDropSession class categoriesFor: #onExit!event handling!private! !
!ShellDragDropSession class categoriesFor: #onStartup!event handling!private! !
!ShellDragDropSession class categoriesFor: #registerDropTarget:!operations!public! !
!ShellDragDropSession class categoriesFor: #reregisterAll!operations!public! !
!ShellDragDropSession class categoriesFor: #revokeDropTarget:!operations!public! !
!ShellDragDropSession class categoriesFor: #uninitialize!initializing!public! !

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

IDropTarget guid: (IID fromString: '{00000122-0000-0000-C000-000000000046}')!
IDropTarget comment: 'IDropTarget is a wrapper class for the IDropTarget COM interface.'!
!IDropTarget categoriesForClass!Unclassified! !
!IDropTarget methodsFor!

DragEnter: dragEnter grfKeyState: grfKeyState pt: pt pdwEffect: pdwEffect
	"Callout for the IDropTarget::DragEnter() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDropTarget>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 4 IDataObject* dword POINTL DWORD*>
	^self invalidCall!

DragLeave
	"Callout for the IDropTarget::DragLeave() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDropTarget>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 6>
	^self invalidCall!

DragOver: dragOver pt: pt pdwEffect: pdwEffect
	"Callout for the IDropTarget::DragOver() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDropTarget>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 5 dword POINTL DWORD*>
	^self invalidCall!

Drop: drop grfKeyState: grfKeyState pt: pt pdwEffect: pdwEffect
	"Callout for the IDropTarget::Drop() interface function.
	N.B. This method has been automatically generated from 
	the vtable defined in IDropTarget>>defineFunctions. DO NOT MODIFY!!"

	<virtual stdcall: hresult 7 IDataObject* dword POINTL DWORD*>
	^self invalidCall! !
!IDropTarget categoriesFor: #DragEnter:grfKeyState:pt:pdwEffect:!COM Interfaces-IDropTarget!public! !
!IDropTarget categoriesFor: #DragLeave!COM Interfaces-IDropTarget!public! !
!IDropTarget categoriesFor: #DragOver:pt:pdwEffect:!COM Interfaces-IDropTarget!public! !
!IDropTarget categoriesFor: #Drop:grfKeyState:pt:pdwEffect:!COM Interfaces-IDropTarget!public! !

!IDropTarget class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'oleidl.IDropTarget'
		IDropTarget  compileFunctions
		IDropTarget  compileDefinition
		IDropTarget  defineTemplate

	"

	self
		defineFunction: #DragEnter:grfKeyState:pt:pdwEffect:
			argumentTypes: 'IDataObject* dword POINTL DWORD*';
		defineFunction: #DragOver:pt:pdwEffect: argumentTypes: 'DWORD POINTL DWORD*';
		defineFunction: #DragLeave argumentTypes: '';
		defineFunction: #Drop:grfKeyState:pt:pdwEffect:
			argumentTypes: 'IDataObject* dword POINTL DWORD*'! !
!IDropTarget class categoriesFor: #defineFunctions!initializing!public! !

IEnumFORMATETC guid: (IID fromString: '{7DD75F87-9857-41E6-8100-9094972ABE93}')!
IEnumFORMATETC comment: 'IEnumFORMATETC is a wrapper class for the IEnumFORMATETC COM interface, and is used by IDataObject (and thus by ShellDragDropSession).  An IEnumFORMATETC instance is a pointer to a COM enumeration object.  It is created by IDataObject>>enumFormatEtc:, which may be accessed through methods in ShellDragDropSession.  Once an IEnumFORMATETC is created, #nextAvailable can be used to enumerate the FORMATETCs of the object.'!
!IEnumFORMATETC categoriesForClass!Unclassified! !
!IEnumFORMATETC class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Iobjidl.IEnumFORMATETC'
		FORMATETC defineTemplate"

	"This method only present to prevent auto-generation from a type library"

	!

elementClass
	"Answer the class of element enumerated by the receiver."

	^FORMATETC! !
!IEnumFORMATETC class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!IEnumFORMATETC class categoriesFor: #elementClass!constants!public! !

DROPFILES guid: (GUID fromString: '{BC6C930B-BD51-4915-8174-8CB74AC7A741}')!
DROPFILES comment: 'DROPFILES is an <ExternalStructure> that wraps the Windows DROPFILES structure.  DROPFILES defines the CF_HDROP and CF_PRINTERS clipboard formats. In the CF_HDROP case, the data that follows is a double null-terminated list of file names. For CF_PRINTERS, the data that follows are the printer friendly names.

Aspects:
- pFiles, offset of the file list from the beginning of this structure, in bytes.
- pt , drop point. The coordinates depend on fNC.
- fNC, nonclient area flag. If true, pt specifies the screen coordinates of a point in a window''s nonclient area. If it is false, pt specifies the client coordinates of a point in the client area.
- fWide, a Boolean that indicates whether the file contains ANSI or Unicode characters. If the value is false, the file contains ANSI characters. Otherwise, it contains Unicode characters.'!
!DROPFILES categoriesForClass!Unclassified! !
!DROPFILES methodsFor!

fNC
	"Answer the receiver's fNC field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asBoolean!

fNC: anObject
	"Set the receiver's fNC field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject asParameter!

fWide
	"Answer the receiver's fWide field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asBoolean!

fWide: anObject
	"Set the receiver's fWide field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject asParameter!

pFiles
	"Answer the receiver's pFiles field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

pFiles: anObject
	"Set the receiver's pFiles field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

pt
	"Answer the receiver's pt field as a Smalltalk object."

	^POINTL fromAddress: (bytes yourAddress + 4)!

pt: anObject
	"Set the receiver's pt field to the value of anObject."

	anObject replaceBytesOf: bytes from: 5 to: 12 startingAt: 1! !
!DROPFILES categoriesFor: #fNC!**compiled accessors**!public! !
!DROPFILES categoriesFor: #fNC:!**compiled accessors**!public! !
!DROPFILES categoriesFor: #fWide!**compiled accessors**!public! !
!DROPFILES categoriesFor: #fWide:!**compiled accessors**!public! !
!DROPFILES categoriesFor: #pFiles!**compiled accessors**!public! !
!DROPFILES categoriesFor: #pFiles:!**compiled accessors**!public! !
!DROPFILES categoriesFor: #pt!**compiled accessors**!public! !
!DROPFILES categoriesFor: #pt:!**compiled accessors**!public! !

!DROPFILES class methodsFor!

defineFields
	"Define the fields of the DROPFILES .

		DROPFILES compileDefinition.

		typedef struct _DROPFILES {
    			DWORD  pFiles;
    			POINT  pt;
    			BOOL  fNC;
    			BOOL  fWide;
			} DROPFILES, *LPDROPFILES;
	"

	self
		defineField: #pFiles type: DWORDField new;
		defineField: #pt type: (StructureField type: POINT);
		defineField: #fNC type: BOOLField new;
		defineField: #fWide type: BOOLField new! !
!DROPFILES class categoriesFor: #defineFields!initializing!public! !

FILEDESCRIPTOR guid: (GUID fromString: '{04E70B60-6256-4473-9451-8C2D02C0A695}')!
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

FILEGROUPDESCRIPTOR guid: (GUID fromString: '{7FAEDAB6-A88C-4DD5-990C-3F9BC8B7D1D0}')!
FILEGROUPDESCRIPTOR comment: 'FILEGROUPDESCRIPTOR is an <ExternalStructure> that wraps the Windows FILEGROUPDESCRIPTOR structure and defines the CF_FILEGROUPDESCRIPTOR clipboard format. 

- cItems, the number of elements in fgd. 
- fgd, an array of FILEDESCRIPTOR structures that contain the file information. 
'!
!FILEGROUPDESCRIPTOR categoriesForClass!Unclassified! !
!FILEGROUPDESCRIPTOR methodsFor!

cItems
	"Answer the receiver's cItems field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cItems: anObject
	"Set the receiver's cItems field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

fgd
	"Answer the receiver's fgd field as a Smalltalk object."

	^StructureArray fromAddress: (bytes yourAddress + 4) length: self cItems elementClass: FILEDESCRIPTOR!

fgd: anObject
	"Set the receiver's fgd field to the value of anObject."

	| size |
	size := anObject byteSize min: (self cItems * 0).
	anObject replaceBytesOf: bytes from: 5 to: 4 + size startingAt: 1! !
!FILEGROUPDESCRIPTOR categoriesFor: #cItems!**compiled accessors**!public! !
!FILEGROUPDESCRIPTOR categoriesFor: #cItems:!**compiled accessors**!public! !
!FILEGROUPDESCRIPTOR categoriesFor: #fgd!**compiled accessors**!public! !
!FILEGROUPDESCRIPTOR categoriesFor: #fgd:!**compiled accessors**!public! !

!FILEGROUPDESCRIPTOR class methodsFor!

defineFields
	"Define the Win32 FILEGROUPDESCRIPTOR structure
		FILEGROUPDESCRIPTOR compileDefinition

	typedef struct _FILEGROUPDESCRIPTOR {
	    UINT cItems; 
	    FILEDESCRIPTOR fgd[1]; 
	} FILEGROUPDESCRIPTOR, * LPFILEGROUPDESCRIPTOR; 


	N.B. #fgd is really a variable length array 
	"

	self
		defineField: #cItems type: DWORDField new;
		defineField: #fgd type: (VariableStructureArrayField type: FILEDESCRIPTOR length: #cItems)! !
!FILEGROUPDESCRIPTOR class categoriesFor: #defineFields!initializing!public! !

FORMATETC guid: (GUID fromString: '{4F9B5D7F-2C30-4FDD-9639-E389A9DEDCBF}')!
FORMATETC comment: 'FORMATETC is an <ExternalStructure> that wraps the Windows FORMATETC structure.

[Edit the following which is almost verbatim from MSDN]

The FORMATETC structure is a generalized Clipboard format. It is enhanced to encompass a target device, the aspect or view of the data, and a storage medium indicator. Where one might expect to find a Clipboard format, OLE uses a FORMATETC data structure instead. This structure is used as a parameter in OLE functions and methods that require data format information.

- cfFormat, a particular clipboard format of interest. The three types of formats recognized by OLE are 1) Standard interchange formats, such as CF_TEXT; 2) Private application formats understood only by the application offering the format, or by other applications offering similar features; and 3) OLE formats, which are used to create linked or embedded objects. 

- ptd, a pointer to a DVTARGETDEVICE structure containing information about the target device for which the data is being composed. A NULL value is used whenever the specified data format is independent of the target device or when the caller doesn''t care what device is used. In the latter case, if the data requires a target device, the object should pick an appropriate default device (often the display for visual components). Data obtained from an object with a NULL target device, such as most metafiles, is independent of the target device. The resulting data is usually the same as it would be if the user chose the Save As command from the File menu and selected an interchange format. 

- dwAspect, one (not one or more) of the DVASPECT enumeration constants that indicate how much detail should be contained in the rendering. A single clipboard format can support multiple aspects or views of the object. Most data and presentation transfer and caching methods pass aspect information. For example, a caller might request an object''s iconic picture, using the metafile clipboard format to retrieve it.

- lindex, part of the aspect when the data must be split across page boundaries. The most common value is -1, which identifies all of the data. For the aspects DVASPECT_THUMBNAIL and DVASPECT_ICON, lindex is ignored. 

- tymed, one of the TYMED enumeration constants which indicate the type of storage medium used to transfer the object''s data. Data can be transferred using whatever medium makes sense for the object. For example, data can be passed using global memory, a disk file, or structured storage objects.'!
!FORMATETC categoriesForClass!Unclassified! !
!FORMATETC methodsFor!

cfFormat
	"Answer the receiver's cfFormat field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

cfFormat: anObject
	"Set the receiver's cfFormat field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject!

dwAspect
	"Answer the receiver's dwAspect field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

dwAspect: anObject
	"Set the receiver's dwAspect field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

lindex
	"Answer the receiver's lindex field as a Smalltalk object."

	^(bytes sdwordAtOffset: 12)!

lindex: anObject
	"Set the receiver's lindex field to the value of anObject."

	bytes sdwordAtOffset: 12 put: anObject!

ptd
	"Answer the receiver's ptd field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

ptd: anObject
	"Set the receiver's ptd field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

tymed
	"Answer the receiver's tymed field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

tymed: anObject
	"Set the receiver's tymed field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!FORMATETC categoriesFor: #cfFormat!**compiled accessors**!public! !
!FORMATETC categoriesFor: #cfFormat:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #dwAspect!**compiled accessors**!public! !
!FORMATETC categoriesFor: #dwAspect:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #lindex!**compiled accessors**!public! !
!FORMATETC categoriesFor: #lindex:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #ptd!**compiled accessors**!public! !
!FORMATETC categoriesFor: #ptd:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #tymed!**compiled accessors**!public! !
!FORMATETC categoriesFor: #tymed:!**compiled accessors**!public! !

!FORMATETC class methodsFor!

defineFields
	"Define the fields of the Win32 FORMATETC structure.

		self compileDefinition

		typedef struct tagFORMATETC 
{ 
    CLIPFORMAT      cfFormat; 
    DVTARGETDEVICE  *ptd; 
    DWORD           dwAspect; 
    LONG            lindex; 
    DWORD           tymed; 
}FORMATETC, *LPFORMATETC;
 
	"

	self
		defineField: #cfFormat type: WORDField new;
		defineField: #ptd type: DWORDField new;
		defineField: #dwAspect type: DWORDField new;
		defineField: #lindex type: SDWORDField new;
		defineField: #tymed type: DWORDField new! !
!FORMATETC class categoriesFor: #defineFields!initializing!public! !

STGMEDIUM guid: (GUID fromString: '{A151B16B-B8FE-4851-9BB2-E17202681E82}')!
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

ShellDragDropSample guid: (GUID fromString: '{AC1DB0E4-AF7F-4950-BD45-66BD0436DCC3}')!
ShellDragDropSample comment: 'ShellDragDropSample is a sample shell to demonstrates the use of Shell data transfer using COM drag and drop.

	ShellDragDropSample  show.'!
!ShellDragDropSample categoriesForClass!Unclassified! !
!ShellDragDropSample methodsFor!

clearAll
	textPresenter model: '' asValue.
	fullTextPresenter model: '' asValue.
	listPresenter clear.
	listBoxPresenter clear!

contentsOf: aFilename
	"self halt."

	| fs contents |
	^
	[fs := FileStream read: aFilename text: true.
	[contents := fs contents] ensure: [fs close].
	contents] 
			on: Error
			do: [:e | '']!

createComponents
	super createComponents.
	textPresenter := self add: TextPresenter new name: 'text'.
	fullTextPresenter := self add: TextPresenter new name: 'fullText'.
	listPresenter := self add: ListPresenter new name: 'list'.
	listBoxPresenter := self add: ListPresenter new name: 'listBox'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	self when: #closeRequested: send: #onCloseRequested: to: self.

	listPresenter
		when: #dragOver:
			send: #onDragOverList:
			to: self;
		when: #drop:
			send: #onDropOverList:
			to: self.
	listBoxPresenter
		when: #dragOver:
			send: #onDragOverListBox:
			to: self;
		when: #drop:
			send: #onDropOverListBox:
			to: self.
	fullTextPresenter
		when: #dragOver:
			send: #onDragOverFullText:
			to: self;
		when: #drop:
			send: #onDropOverFullText:
			to: self!

helpAbout
	(MessageBox new)
		caption: 'About ' , self class name asString;
		icon: self class icon;
		text: self class aboutText;
		open!

onCloseRequested
	ShellDragDropSession revokeDropTarget: self view.
	^super onCloseRequested!

onCloseRequested: aValue
	ShellDragDropSession revokeDropTarget: self view.
	aValue value: true!

onDragOverFullText: aDragDropSession

	aDragDropSession 
		operation: ((aDragDropSession dragObjects 
				allSatisfy: [:each | (each isFormatAvailable: #Filenames) and: [(each format: #Filenames) size = 1]]) 
					ifTrue: [aDragDropSession intendedOperation]
					ifFalse: [])!

onDragOverList: aDragDropSession
	aDragDropSession operation: ((aDragDropSession isFormatAvailable: #Filenames) 
				ifTrue: [aDragDropSession intendedOperation]
				ifFalse: [])!

onDragOverListBox: aDragDropSession
	aDragDropSession 
		operation: ((aDragDropSession dragObjects 
				allSatisfy: [:each | (each isFormatAvailable: #Filenames) and: [(each format: #Filenames) size = 1]]) 
					ifTrue: [aDragDropSession intendedOperation]
					ifFalse: [])!

onDropOverFullText: aSession
	| object |
	aSession dragObjects do: 
			[:each | 
			(each isFormatAvailable: #Filenames) 
				ifTrue: 
					[object := each format: #Filenames.
					each format: #String
						data: (object isEmpty ifTrue: [''] ifFalse: [self contentsOf: object first])]]!

onDropOverList: aSession
	aSession dragObjects do: 
			[:each | 
			(each isFormatAvailable: #Filenames) 
				ifTrue: 
					[| object |
					object := each format: #Filenames.
					aSession suggestedTarget isNil 
						ifTrue: [listPresenter model addAll: object]
						ifFalse: [listPresenter model addAll: object after: aSession suggestedTarget]]]!

onDropOverListBox: aSession
	aSession dragObjects do: 
			[:each | 
			(each isFormatAvailable: #Filenames) 
				ifTrue: 
					[| object |
					object := each format: #Filenames.
					aSession suggestedTarget isNil 
						ifTrue: [listBoxPresenter model addAll: object]
						ifFalse: [listBoxPresenter model addAll: object after: aSession suggestedTarget]]]!

onViewOpened
	super onViewOpened.
	ShellDragDropSession registerDropTarget: self view! !
!ShellDragDropSample categoriesFor: #clearAll!commands!public! !
!ShellDragDropSample categoriesFor: #contentsOf:!event handling!public! !
!ShellDragDropSample categoriesFor: #createComponents!initializing!public! !
!ShellDragDropSample categoriesFor: #createSchematicWiring!initializing!public! !
!ShellDragDropSample categoriesFor: #helpAbout!commands!public! !
!ShellDragDropSample categoriesFor: #onCloseRequested!event handling!public! !
!ShellDragDropSample categoriesFor: #onCloseRequested:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDragOverFullText:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDragOverList:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDragOverListBox:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDropOverFullText:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDropOverList:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDropOverListBox:!event handling!public! !
!ShellDragDropSample categoriesFor: #onViewOpened!event handling!public! !

!ShellDragDropSample class methodsFor!

aboutText
	^'ShellDragDropSample 

Copyright: 2002 
  Louis Sumberg <lsumberg@mindspring.com>
  Steve Waring <swaring@ozemail.com.au>

Version: %1 

DISCLAIMER: This software is freely provided purely as a sample and as such it
is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE' 
		formatWith: SessionManager current versionInfo productVersionString!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 3 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #exit 8 'E&xit' 1 1 0 0 0 8 '&File' 0 1 0 0 23679 0 0 658 0 16 98 1 706 1 738 8 #clearAll 8 '&Clear All' 1 1 0 0 0 8 '&Edit' 0 1 0 0 23683 0 0 658 0 16 98 1 706 1 738 8 #helpAbout 8 'About' 1 1 0 0 0 8 'Help' 0 1 0 0 23687 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  89 117 1186 2101 1449 416 1122 8 #text: 98 1 8 'Shell DragDrop Sample' 416 1122 8 #updateMenuBar 576 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 0 0 0 58 0 0 0 70 4 0 0 14 3 0 0] 98 3 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 1392 0 482 512 0 5 0 0 0 1392 530 234 240 576 32 234 256 576 0 1058 202 208 98 1 1122 1152 98 2 1186 1 1 1186 1173 199 1392 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 74 2 0 0 99 0 0 0] 98 3 410 1408 98 15 0 1392 98 2 8 1140850688 131073 1696 0 482 512 0 5 0 0 0 1696 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.StaticText)  98 16 0 1696 98 2 8 1140850945 1 1808 0 0 0 5 0 0 0 1808 0 8 4294902319 852486 ##(Smalltalk.NullConverter)  0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 583 51 1808 1122 1248 98 1 8 'First Filename' 1808 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 98 0 1186 193 193 0 27 0 0 0 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 1696 98 2 8 1143017796 1025 2160 0 482 512 0 21 0 0 0 2160 0 8 4294902701 1906 0 0 9 1058 202 208 98 3 1122 1152 98 2 1186 1 51 1186 583 149 2160 1122 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2160 1122 8 #isTextModified: 98 1 32 2160 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 35 1 0 0 99 0 0 0] 98 0 2144 0 27 234 256 98 2 2160 8 'text' 0 1058 202 208 98 1 1122 1152 98 2 1186 1 1 1186 583 199 1696 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 99 0 0 0] 98 2 1808 2160 2144 0 27 410 8 ##(Smalltalk.Splitter)  98 12 0 1392 98 2 8 1140850688 1 2784 0 482 512 0 517 0 0 0 2784 1058 202 208 98 1 1122 1152 98 2 1186 583 1 1186 7 199 2784 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 1 0 0 0 0 0 0 38 1 0 0 99 0 0 0] 98 0 2144 0 27 410 1408 98 15 0 1392 98 2 8 1140850688 131073 3040 0 482 512 0 5 0 0 0 3040 1778 1 1 410 1824 98 16 0 3040 98 2 8 1140850945 1 3136 0 0 0 5 0 0 0 3136 0 8 4294902319 1906 0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 585 51 3136 1122 1248 98 1 8 'First Contents (one file only)' 3136 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 36 1 0 0 25 0 0 0] 98 0 2144 0 27 0 0 0 410 2176 98 16 0 3040 98 2 8 1143017796 1025 3440 0 482 512 0 21 0 0 0 3440 0 8 4294902701 1906 0 0 9 1058 202 208 98 3 1122 1152 98 2 1186 1 51 1186 585 149 3440 1122 2416 98 1 2450 3 1 3 3440 1122 2496 98 1 32 3440 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 36 1 0 0 99 0 0 0] 98 0 2144 0 27 234 256 98 2 3440 8 'fullText' 0 1058 202 208 98 1 1122 1152 98 2 1186 589 1 1186 585 199 3040 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 1 0 0 0 0 0 0 74 2 0 0 99 0 0 0] 98 2 3136 3440 2144 0 27 2144 0 27 410 2800 98 12 0 416 98 2 8 1140850688 1 4000 0 482 512 0 517 0 0 0 4000 1058 202 208 98 1 1122 1152 98 2 1186 1 199 1186 1173 7 4000 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 99 0 0 0 74 2 0 0 102 0 0 0] 98 0 2144 0 27 410 1408 98 15 0 416 98 2 8 1140850688 131073 4240 0 482 512 0 5 0 0 0 4240 530 234 240 576 32 234 256 576 0 1058 202 208 98 1 1122 1152 98 2 1186 1 205 1186 1173 201 4240 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 102 0 0 0 74 2 0 0 202 0 0 0] 98 3 410 1408 98 15 0 4240 98 2 8 1140850688 131073 4528 0 482 512 0 5 0 0 0 4528 1778 1 1 410 1824 98 16 0 4528 98 2 8 1140850945 1 4624 0 0 0 5 0 0 0 4624 0 8 4294902319 1906 0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 583 51 4624 1122 1248 98 1 8 'Add Filenames' 4624 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 98 0 2144 0 27 0 0 0 410 8 ##(Smalltalk.ListView)  98 30 0 4528 98 2 8 1140920397 1025 4928 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 21 0 0 0 4928 0 8 4294902199 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  5066 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Column 1' 575 8 #left 5152 8 ##(Smalltalk.SortedCollection)  0 0 4928 0 3 0 0 8 #report 576 0 131169 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 51 1186 583 151 4928 1122 1248 98 1 8 'Column 1' 4928 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 35 1 0 0 100 0 0 0] 98 0 2144 0 27 234 256 98 2 4928 8 'list' 0 1058 202 208 98 1 1122 1152 98 2 1186 1 1 1186 583 201 4528 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 100 0 0 0] 98 2 4624 4928 2144 0 27 410 2800 98 12 0 4240 98 2 8 1140850688 1 5776 0 482 512 0 517 0 0 0 5776 1058 202 208 98 1 1122 1152 98 2 1186 583 1 1186 7 201 5776 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 1 0 0 0 0 0 0 38 1 0 0 100 0 0 0] 98 0 2144 0 27 410 1408 98 15 0 4240 98 2 8 1140850688 131073 6016 0 482 512 0 5 0 0 0 6016 1778 1 1 410 1824 98 16 0 6016 98 2 8 1140850945 1 6112 0 0 0 5 0 0 0 6112 0 8 4294902319 1906 0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 585 51 6112 1122 1248 98 1 8 'Add Filename (one file only)' 6112 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 36 1 0 0 25 0 0 0] 98 0 2144 0 27 0 0 0 410 8 ##(Smalltalk.ListBox)  98 17 0 6016 98 2 8 1144062209 1025 6416 5010 202 208 576 0 5066 5088 5104 482 512 0 21 0 0 0 6416 0 8 4294902777 5152 576 32 1058 202 208 98 2 1122 1152 98 2 1186 1 51 1186 585 151 6416 1122 8 #horizontalExtent: 98 1 1 6416 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 36 1 0 0 100 0 0 0] 98 0 2144 0 27 234 256 98 2 6416 8 'listBox' 0 1058 202 208 98 1 1122 1152 98 2 1186 589 1 1186 585 201 6016 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 1 0 0 0 0 0 0 74 2 0 0 100 0 0 0] 98 2 6112 6416 2144 0 27 2144 0 27 2144 0 27 )! !
!ShellDragDropSample class categoriesFor: #aboutText!enquiries!public! !
!ShellDragDropSample class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

