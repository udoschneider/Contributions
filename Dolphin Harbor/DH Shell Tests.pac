| package |
package := Package name: 'DH Shell Tests'.
package paxVersion: 0;
	basicComment: ''.

package basicPackageVersion: '5.1.2.2'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIAAAAAA=').

package classNames
	add: #ITEMIDLISTTest;
	add: #ShellFolderTest;
	add: #ShellObjectResource;
	add: #ShellObjectTest;
	add: #ShellObjectTests;
	add: #SystemImageManagerTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'DH Shell Core';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #ShellObjectTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #SystemImageManagerTest
	instanceVariableNames: 'manager'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellObjectTests subclass: #ITEMIDLISTTest
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellObjectTests subclass: #ShellFolderTest
	instanceVariableNames: 'shellFolder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellObjectTests subclass: #ShellObjectTest
	instanceVariableNames: 'shellObject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestResource subclass: #ShellObjectResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ShellObjectTests guid: (GUID fromString: '{04808DD7-E3FD-4885-B1E2-3084AEFA126A}')!
ShellObjectTests comment: ''!
!ShellObjectTests categoriesForClass!Unclassified! !
!ShellObjectTests methodsFor!

pathOfEmptyFolder
	^ShellObjectResource current pathOfEmptyFolder!

pathOfFolderWithFile
	^ShellObjectResource current pathOfFolderWithFile!

pathOfTempFile
	^ShellObjectResource current pathOfTempFile!

pathOfTempRoot
	^ShellObjectResource current pathOfTempRoot!

pathOfTestDrive
	^ShellObjectResource current pathOfTestDrive! !
!ShellObjectTests categoriesFor: #pathOfEmptyFolder!constants!public! !
!ShellObjectTests categoriesFor: #pathOfFolderWithFile!constants!public! !
!ShellObjectTests categoriesFor: #pathOfTempFile!constants!public! !
!ShellObjectTests categoriesFor: #pathOfTempRoot!constants!public! !
!ShellObjectTests categoriesFor: #pathOfTestDrive!constants!public! !

!ShellObjectTests class methodsFor!

accessGetters
	^#(#displayName #displayNormalName #displayParsingName #flags #fileSize #hasSubFolders #isFileSystem #isDesktop #lastWriteTime #parent #sysSmallIconIndex #sysSmallOpenIconIndex #typeName #lastWriteTime #isFolder #fileSize) 
		asOrderedCollection!

resources
	^Array with: ShellObjectResource! !
!ShellObjectTests class categoriesFor: #accessGetters!accessing!public! !
!ShellObjectTests class categoriesFor: #resources!accessing!public! !

SystemImageManagerTest guid: (GUID fromString: '{31CA8764-4898-4C05-A41A-B8DCC0FDF7E6}')!
SystemImageManagerTest comment: ''!
!SystemImageManagerTest categoriesForClass!Unclassified! !
!SystemImageManagerTest methodsFor!

setUp
	super setUp.
	manager := SystemImageManager basicNew initialize!

testAddImageException
	"self 
		should: [manager addImage: 'foo']
		raise: Error
		matching: [:ex | ex messageText = 'Can not add images to the system image list']"

	!

testImageListWithExtent
	self assert: manager imageLists isEmpty.
	manager imageListWithExtent: Icon smallExtent.
	self assert: (manager imageLists at: Icon smallExtent) class == WinImageList.
	manager purge.
	self assert: manager imageLists isEmpty.
	manager imageListWithExtent: Icon smallExtent.
	self assert: (manager imageLists at: Icon smallExtent) class == WinImageList!

testIsLargeExtent
	self assert: (manager isExtentLarge: Icon largeExtent).
	self deny: (manager isExtentLarge: Icon smallExtent)!

testLargeExtent
	| large small |
	self assert: manager imageLists isEmpty.
	large := manager imageListWithExtent: Icon largeExtent.
	small := manager imageListWithExtent: Icon smallExtent.
	self deny: large == small!

testSTB
	| bytes restored winImageList |
	manager imageListWithExtent: Icon smallExtent.
	bytes := manager binaryStoreBytes.
	restored := Object fromBinaryStoreBytes: bytes.
	winImageList := restored imageListWithExtent: Icon smallExtent.
	self assert: winImageList class == WinImageList.
	self assert: winImageList isRealized! !
!SystemImageManagerTest categoriesFor: #setUp!public!setup! !
!SystemImageManagerTest categoriesFor: #testAddImageException!public!tests! !
!SystemImageManagerTest categoriesFor: #testImageListWithExtent!public!tests! !
!SystemImageManagerTest categoriesFor: #testIsLargeExtent!public!tests! !
!SystemImageManagerTest categoriesFor: #testLargeExtent!public!tests! !
!SystemImageManagerTest categoriesFor: #testSTB!public!tests! !

ITEMIDLISTTest guid: (GUID fromString: '{AB7F7360-F5BA-4ABB-8DEE-F209B30FEE53}')!
ITEMIDLISTTest comment: ''!
!ITEMIDLISTTest categoriesForClass!Unclassified! !
!ITEMIDLISTTest methodsFor!

testAppend
	| newId relativeId folder |
	folder := ShellFolder fromPath: self pathOfTempRoot.
	id := folder idFull.
	relativeId := (folder folderInterface childrenWithFlags: 96) first.
	newId := id append: relativeId.
	self assert: id ~~ newId.
	self assert: newId getSize = (id getSize + relativeId getSize)!

testDesktopID
	self assert: ITEMIDLIST newBuffer isDesktopID.
	self assert: ShellFolder desktop idFull isDesktopID.
	self assert: ITEMIDLIST newBuffer idNext isNil!

testIdLast
	| idLast |
	id := (ShellFolder fromPath: self pathOfEmptyFolder) idFull.
	idLast := id idLast.
	self assert: idLast isSingle.
	self deny: id isSingle.
	self 
		assert: (idLast back == id or: [idLast back back == id or: [idLast back back back == id]])!

testIdNext
	| idLast |
	id := (ShellFolder fromPath: self pathOfEmptyFolder) idFull.
	idLast := id idLast.
	self assert: idLast idNext isNil.
	self deny: id idNext isNil.
	self assert: id idNext back == id!

testIdParent
	| idLast idParent |
	id := (ShellFolder fromPath: self pathOfEmptyFolder) idFull.
	idLast := id idLast.
	idParent := id idParent.
	self assert: id getSize = (idParent append: idLast) getSize! !
!ITEMIDLISTTest categoriesFor: #testAppend!public!unit tests! !
!ITEMIDLISTTest categoriesFor: #testDesktopID!public!unit tests! !
!ITEMIDLISTTest categoriesFor: #testIdLast!public!unit tests! !
!ITEMIDLISTTest categoriesFor: #testIdNext!public!unit tests! !
!ITEMIDLISTTest categoriesFor: #testIdParent!public!unit tests! !

ShellFolderTest guid: (GUID fromString: '{C61A1F49-3DBF-402B-BECB-534BB0908252}')!
ShellFolderTest comment: ''!
!ShellFolderTest categoriesForClass!Unclassified! !
!ShellFolderTest methodsFor!

assertEmptyFolder
	self assert: shellFolder displayName = 'emptyFolder'.
	self assert: shellFolder displayParsingName = self pathOfEmptyFolder.
	self assert: (shellFolder sysSmallIconIndex isKindOf: Integer).
	self assert: (shellFolder sysSmallOpenIconIndex isKindOf: Integer).
	self deny: shellFolder hasSubFolders.
	self assert: shellFolder subFolderNames asArray = #().
	self assert: shellFolder subFileNames asSortedCollection asArray = #().
	self assert: shellFolder isFileSystem!

assertIDFullRoundTrip
	| newShellFolder |
	newShellFolder := ShellFolder onIDFull: shellFolder idFull.
	self assertIsEqual: newShellFolder!

assertIsEqual: newShellFolder
	self assert: shellFolder = shellFolder.
	self assert: shellFolder <= shellFolder.
	self assert: newShellFolder = shellFolder.
	self assert: newShellFolder <= shellFolder!

assertParentChild
	self assert: (shellFolder parent subFolders includes: shellFolder)!

assertParsingRoundTrip
	| newShellFolder |
	newShellFolder := ShellFolder fromParseName: shellFolder parseName.
	self assertIsEqual: newShellFolder!

assertRootFolder
	self assert: shellFolder displayName = 'TempLASSWShellTest'.
	self assert: shellFolder displayParsingName = self pathOfTempRoot.
	self assert: (shellFolder sysSmallIconIndex isKindOf: Integer).
	self assert: (shellFolder sysSmallOpenIconIndex isKindOf: Integer).
	self assert: shellFolder hasSubFolders.
	self assert: shellFolder subFolderNames asSortedCollection asArray 
				= #('emptyFolder' 'folderWithFile').
	self assert: shellFolder subFileNames asSortedCollection asArray = #().
	self assert: shellFolder isFileSystem.
	shellFolder subObjects do: [:each | self assert: (each isKindOf: ShellFolder)]!

assertWithFileFolder
	self assert: shellFolder displayParsingName = self pathOfFolderWithFile.
	self assert: (shellFolder sysSmallIconIndex isKindOf: Integer).
	self assert: (shellFolder sysSmallOpenIconIndex isKindOf: Integer).
	self deny: shellFolder hasSubFolders.
	self assert: shellFolder subFolderNames asSortedCollection asArray = #().
	self assert: shellFolder subFileNames asSortedCollection asArray = #('testFile.tmp').
	self assert: shellFolder displayName = 'folderWithFile'.
	self assert: shellFolder isFileSystem.
	shellFolder subObjects do: [:each | self assert: (each isKindOf: ShellObject)]!

testDesktop
	self class access: (shellFolder := ShellFolder desktop).
	self assert: shellFolder displayName = 'Desktop'.
	self assert: shellFolder parseName = 'Desktop'.
	self assert: shellFolder isDesktop.
	self assert: shellFolder = ShellFolder desktop.
	self assert: shellFolder <= ShellFolder desktop.
	self assert: shellFolder parent isNil.
	self assertParsingRoundTrip!

testEmptyFromFull
	shellFolder := ShellFolder fromPath: self pathOfEmptyFolder.
	self assertEmptyFolder!

testEmptyFromRelative
	| root |
	root := ShellFolder fromPath: self pathOfTempRoot.
	shellFolder := root subFolders 
				detect: [:each | each displayParsingName = self pathOfEmptyFolder].
	self assertEmptyFolder!

testOnStartUp
	shellFolder := ShellFolder fromPath: self pathOfTempRoot.
	self assert: shellFolder isValid.
	shellFolder onStartup.
	self deny: shellFolder isValid!

testProgramFilesFromPath
	shellFolder := ShellFolder fromPath: 'c:\program files'.
	self assert: shellFolder displayName = 'Program Files'.
!

testProgramFilesSub
	| subFolders |
	shellFolder := ShellFolder fromPath: 'c:\program files'.
	subFolders := shellFolder subFolderNames asSortedCollection.
	self assert: subFolders notEmpty.
	subFolders do: [:each | self assert: each class == String].
	self deny: shellFolder isDesktop.
	self assert: (shellFolder parent isKindOf: ShellObject)!

testRootFromFull
	shellFolder := ShellFolder fromPath: self pathOfTempRoot.
	self assertRootFolder!

testRootFromRelative
	| root |
	root := ShellFolder fromPath: self pathOfTestDrive.
	shellFolder := root subFolders 
				detect: [:each | each displayParsingName = self pathOfTempRoot].
	self assertRootFolder!

testSpecialDesktopDirectory
	self class access: (shellFolder := ShellFolder desktopDirectory).
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	shellFolder isWin9X ifFalse: [self assertParentChild]!

testSpecialFavorites
	self class access: (shellFolder := ShellFolder favorites).
	self assert: shellFolder displayName = 'Favorites'.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testSpecialMyDocuments
	self class access: (shellFolder := ShellFolder myDocuments).
	self assert: shellFolder displayName = 'My Documents'.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testSpecialProgramFiles
	self class access: (shellFolder := ShellFolder programFiles).
	self assert: shellFolder displayName = 'Program Files'.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testSpecialWindows
	self class access: (shellFolder := ShellFolder windows).
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testVirtualControlPanel
	self class access: (shellFolder := ShellFolder controlPanel).
	self assert: shellFolder displayName = 'Control Panel'.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testVirtualMyComputer
	self class access: (shellFolder := ShellFolder myComputer).
	self assert: shellFolder displayName = 'My Computer'.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testWithFileFromFull
	shellFolder := ShellFolder fromPath: self pathOfFolderWithFile.
	self assertWithFileFolder.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild!

testWithFileFromRelative
	| root |
	root := ShellFolder fromPath: self pathOfTempRoot.
	shellFolder := root subFolders 
				detect: [:each | each displayParsingName = self pathOfFolderWithFile].
	self assertWithFileFolder.
	self assertParsingRoundTrip.
	self assertIDFullRoundTrip.
	self assertParentChild! !
!ShellFolderTest categoriesFor: #assertEmptyFolder!asserting!public! !
!ShellFolderTest categoriesFor: #assertIDFullRoundTrip!asserting!public! !
!ShellFolderTest categoriesFor: #assertIsEqual:!asserting!public! !
!ShellFolderTest categoriesFor: #assertParentChild!asserting!public! !
!ShellFolderTest categoriesFor: #assertParsingRoundTrip!asserting!public! !
!ShellFolderTest categoriesFor: #assertRootFolder!asserting!public! !
!ShellFolderTest categoriesFor: #assertWithFileFolder!asserting!public! !
!ShellFolderTest categoriesFor: #testDesktop!public!tests! !
!ShellFolderTest categoriesFor: #testEmptyFromFull!public!tests! !
!ShellFolderTest categoriesFor: #testEmptyFromRelative!public!tests! !
!ShellFolderTest categoriesFor: #testOnStartUp!public!tests! !
!ShellFolderTest categoriesFor: #testProgramFilesFromPath!public!tests! !
!ShellFolderTest categoriesFor: #testProgramFilesSub!public!tests! !
!ShellFolderTest categoriesFor: #testRootFromFull!public!tests! !
!ShellFolderTest categoriesFor: #testRootFromRelative!public!tests! !
!ShellFolderTest categoriesFor: #testSpecialDesktopDirectory!public!tests! !
!ShellFolderTest categoriesFor: #testSpecialFavorites!public!tests! !
!ShellFolderTest categoriesFor: #testSpecialMyDocuments!public!tests! !
!ShellFolderTest categoriesFor: #testSpecialProgramFiles!public!tests! !
!ShellFolderTest categoriesFor: #testSpecialWindows!public!tests! !
!ShellFolderTest categoriesFor: #testVirtualControlPanel!public!tests! !
!ShellFolderTest categoriesFor: #testVirtualMyComputer!public!tests! !
!ShellFolderTest categoriesFor: #testWithFileFromFull!public!tests! !
!ShellFolderTest categoriesFor: #testWithFileFromRelative!public!tests! !

!ShellFolderTest class methodsFor!

access: aShellFolder
	| answers |
	answers := IdentityDictionary new.
	((self accessGetters)
		addAll: #(#subFolderNames #subFileNames #allParents);
		yourself) do: [:each | answers at: each put: (aShellFolder perform: each)].
	^answers! !
!ShellFolderTest class categoriesFor: #access:!helpers!public! !

ShellObjectTest guid: (GUID fromString: '{ECFD6636-7773-483E-9725-78132A7A302E}')!
ShellObjectTest comment: ''!
!ShellObjectTest categoriesForClass!Unclassified! !
!ShellObjectTest methodsFor!

assertTempFile
	self assert: shellObject displayParsingName = self pathOfTempFile.
	self assert: (shellObject sysSmallIconIndex isKindOf: Integer).
	self assert: (shellObject sysSmallOpenIconIndex isKindOf: Integer).
	self deny: shellObject hasSubFolders.
	self assert: shellObject displayName = 'testFile.tmp'.
	self assert: shellObject isFileSystem!

sortTest: aShellObject withAll: aCollection
	| lessOrEqual greater |
	greater := OrderedCollection new.
	lessOrEqual := OrderedCollection new.
	aCollection 
		do: [:each | (each <= aShellObject ifTrue: [lessOrEqual] ifFalse: [greater]) add: each].
	"stability"
	self assert: (lessOrEqual allSatisfy: [:each | each <= aShellObject]).
	self deny: (greater anySatisfy: [:each | each <= aShellObject]).
	"commutative"
	self assert: (greater allSatisfy: [:each | aShellObject <= each]).
	self assert: (lessOrEqual select: [:each | aShellObject <= each]) size = 1!

testFileFromFull
	shellObject := ShellObject fromPath: self pathOfTempFile.
	self assertTempFile!

testFileFromRelative
	| root |
	root := ShellFolder fromPath: self pathOfFolderWithFile.
	shellObject := root subFiles 
				detect: [:each | each displayParsingName = self pathOfTempFile].
	self assertTempFile!

testSorting
	| allFolders |
	allFolders := ShellFolder desktop subFolders.
	allFolders do: [ :each | self sortTest: each withAll: allFolders ]
	!

testStatusString
	shellObject := ShellObject fromPath: self pathOfTempFile.
	self assert: (ShellObject statusStringForAll: #()) = '0 object(s)  0 KB'.
	self 
		assert: (ShellObject statusStringForAll: (Array with: shellObject)) = '1 object(s)  0 KB'.
	self 
		assert: (ShellObject statusStringForAll: (Array with: shellObject with: shellObject)) 
				= '2 object(s)  0 KB'! !
!ShellObjectTest categoriesFor: #assertTempFile!asserting!public! !
!ShellObjectTest categoriesFor: #sortTest:withAll:!helpers!public! !
!ShellObjectTest categoriesFor: #testFileFromFull!public!tests! !
!ShellObjectTest categoriesFor: #testFileFromRelative!public!tests! !
!ShellObjectTest categoriesFor: #testSorting!public!tests! !
!ShellObjectTest categoriesFor: #testStatusString!public!tests! !

!ShellObjectTest class methodsFor!

access: aShellObject
	| answers |
	answers := IdentityDictionary new.
	self accessGetters do: [:each | answers at: each put: (aShellObject perform: each)].
	^answers
! !
!ShellObjectTest class categoriesFor: #access:!helpers!public! !

ShellObjectResource guid: (GUID fromString: '{E6A49442-C788-4937-B93A-17953F6B984C}')!
ShellObjectResource comment: ''!
!ShellObjectResource categoriesForClass!Unclassified! !
!ShellObjectResource methodsFor!

pathOfEmptyFolder
	^'C:\TempLASSWShellTest\emptyFolder'!

pathOfFolderWithFile
	^'C:\TempLASSWShellTest\folderWithFile'!

pathOfTempFile
	^'C:\TempLASSWShellTest\folderWithFile\testFile.tmp'!

pathOfTempRoot
	^'C:\TempLASSWShellTest'!

pathOfTestDrive
	^'C:\'!

setUp
	| fs |
	super setUp.
	File createDirectory: self pathOfTempRoot.
	File createDirectory: self pathOfEmptyFolder.
	File createDirectory: self pathOfFolderWithFile.
	fs := FileStream write: 'c:\TempLASSWShellTest\folderWithFile\testFile.tmp'.
	[fs nextPutAll: 'hi'] ensure: [fs close]!

tearDown
	| fs |
	super tearDown.
	[File deleteDirectory: self pathOfTempRoot] on: Error do: [:e | ]! !
!ShellObjectResource categoriesFor: #pathOfEmptyFolder!constants!public! !
!ShellObjectResource categoriesFor: #pathOfFolderWithFile!constants!public! !
!ShellObjectResource categoriesFor: #pathOfTempFile!constants!public! !
!ShellObjectResource categoriesFor: #pathOfTempRoot!constants!public! !
!ShellObjectResource categoriesFor: #pathOfTestDrive!constants!public! !
!ShellObjectResource categoriesFor: #setUp!public!setup! !
!ShellObjectResource categoriesFor: #tearDown!public!setup! !

"Binary Globals"!

"Resources"!

