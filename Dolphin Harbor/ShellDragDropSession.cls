"Filed out from Dolphin Smalltalk 7"!

DragDropSession subclass: #ShellDragDropSession
	instanceVariableNames: 'keyState dataObject operationDescriptions registeredView'
	classVariableNames: 'Register'
	poolDictionaries: 'OLEConstants OLEShellDataTransferConstants Win32Errors'
	classInstanceVariableNames: ''!
ShellDragDropSession guid: (GUID fromString: '{93e57526-c0be-48a8-9881-18746a7174d6}')!
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

