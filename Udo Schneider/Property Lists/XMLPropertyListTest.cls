"Filed out from Dolphin Smalltalk 7"!

PropertyListTest subclass: #XMLPropertyListTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLPropertyListTest guid: (GUID fromString: '{f495451f-1f75-4fb2-b6c1-a2f9586deadc}')!
XMLPropertyListTest comment: 'SUnitBrowser openOnTestCase: XMLPropertyListTest'!
!XMLPropertyListTest categoriesForClass!Unclassified! !
!XMLPropertyListTest class methodsFor!

filesAndMethods
	^##((Dictionary new)
		at: 'array.xml' put: 'plistArray';
		at: 'array_nested.xml' put: 'plistNestedArray';
		at: 'data.xml' put: 'plistData';
		at: 'date.xml' put: 'plistDate';
		at: 'dictionary.xml' put: 'plistDictionary';
		at: 'dictionary_nested.xml' put: 'plistNestedDictionary';
		at: 'false.xml' put: 'plistFalse';
		at: 'integer.xml' put: 'plistInteger';
		at: 'nplist.xml' put: 'plistNotAPlist';
		at: 'plist_v2.xml' put: 'plistWrongVersion';
		at: 'real.xml' put: 'plistReal';
		at: 'string.xml' put: 'plistString';
		at: 'string_dquotes.xml' put: 'plistStringWithDoubleQuotes';
		at: 'string_squotes.xml' put: 'plistStringWithSingleQuotes';
		at: 'true.xml' put: 'plistTrue';
		yourself)!

plistArray
	"
		self createPlistMethod: 'plistArray' fromFile: 'array.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8YXJyYXk+DQoJPHN0cmluZz5UaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhl
IGxhenkgZG9nJ3MgYmFjazwvc3RyaW5nPg0KCTxpbnRlZ2VyPjgxNTwvaW50ZWdlcj4NCgk8cmVh
bD4zLjE0MTU5MjwvcmVhbD4NCgk8dHJ1ZS8+DQoJPGZhbHNlLz4NCgk8ZGF0ZT4yMDA1LTA3LTI1
VDA4OjI5OjA5WjwvZGF0ZT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8L2RhdGE+DQo8L2Fy
cmF5Pg0KPC9wbGlzdD4NCg==')).!

plistData
	"
		self createPlistMethod: 'plistData' fromFile: 'data.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGF0YT4NCkFTTkZaNG1yemU4PQ0KPC9kYXRhPg0KPC9wbGlzdD4NCg==')).!

plistDate
	"
		self createPlistMethod: 'plistDate' fromFile: 'date.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGF0ZT4yMDA1LTA3LTI1VDA4OjI5OjA5WjwvZGF0ZT4NCjwvcGxpc3Q+DQo=')).!

plistDictionary
	"
		self createPlistMethod: 'plistDictionary' fromFile: 'dictionary.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGljdD4NCgk8a2V5PmRhdGE8L2tleT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8
L2RhdGE+DQoJPGtleT5kYXRlPC9rZXk+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2Rh
dGU+DQoJPGtleT5mYWxzZTwva2V5Pg0KCTxmYWxzZS8+DQoJPGtleT5udW1iZXI8L2tleT4NCgk8
aW50ZWdlcj44MTU8L2ludGVnZXI+DQoJPGtleT5yZWFsPC9rZXk+DQoJPHJlYWw+My4xNDE1OTI8
L3JlYWw+DQoJPGtleT5zdHJpbmc8L2tleT4NCgk8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gg
anVtcHMgb3ZlciB0aGUgbGF6eSBkb2cncyBiYWNrPC9zdHJpbmc+DQoJPGtleT50cnVlPC9rZXk+
DQoJPHRydWUvPg0KPC9kaWN0Pg0KPC9wbGlzdD4NCg==')).!

plistFalse
	"
		self createPlistMethod: 'plistFalse' fromFile: 'false.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZmFsc2UvPg0KPC9wbGlzdD4NCg==')).!

plistInteger
	"
		self createPlistMethod: 'plistInteger' fromFile: 'integer.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8aW50ZWdlcj44MTU8L2ludGVnZXI+DQo8L3BsaXN0Pg0K')).!

plistNestedArray
	"
		self createPlistMethod: 'plistNestedArray' fromFile: 'array_nested.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8YXJyYXk+DQoJPHN0cmluZz5UaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhl
IGxhenkgZG9nJ3MgYmFjazwvc3RyaW5nPg0KCTxpbnRlZ2VyPjgxNTwvaW50ZWdlcj4NCgk8cmVh
bD4zLjE0MTU5MjwvcmVhbD4NCgk8dHJ1ZS8+DQoJPGZhbHNlLz4NCgk8ZGF0ZT4yMDA1LTA3LTI1
VDA4OjI5OjA5WjwvZGF0ZT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8L2RhdGE+DQo8ZGlj
dD4NCgk8a2V5PmRhdGE8L2tleT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8L2RhdGE+DQoJ
PGtleT5kYXRlPC9rZXk+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2RhdGU+DQoJPGtl
eT5mYWxzZTwva2V5Pg0KCTxmYWxzZS8+DQoJPGtleT5udW1iZXI8L2tleT4NCgk8aW50ZWdlcj44
MTU8L2ludGVnZXI+DQoJPGtleT5yZWFsPC9rZXk+DQoJPHJlYWw+My4xNDE1OTI8L3JlYWw+DQoJ
PGtleT5zdHJpbmc8L2tleT4NCgk8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3Zl
ciB0aGUgbGF6eSBkb2cncyBiYWNrPC9zdHJpbmc+DQoJPGtleT50cnVlPC9rZXk+DQoJPHRydWUv
Pg0KCTxrZXk+YXJyYXk8L2tleT4NCgk8YXJyYXk+DQoJPHN0cmluZz5UaGUgcXVpY2sgYnJvd24g
Zm94IGp1bXBzIG92ZXIgdGhlIGxhenkgZG9nJ3MgYmFjazwvc3RyaW5nPg0KCTxpbnRlZ2VyPjgx
NTwvaW50ZWdlcj4NCgk8cmVhbD4zLjE0MTU5MjwvcmVhbD4NCgk8dHJ1ZS8+DQoJPGZhbHNlLz4N
Cgk8ZGF0ZT4yMDA1LTA3LTI1VDA4OjI5OjA5WjwvZGF0ZT4NCgk8ZGF0YT4NCglBU05GWjRtcnpl
OD0NCgk8L2RhdGE+DQoJPC9hcnJheT4NCjwvZGljdD4NCjwvYXJyYXk+DQo8L3BsaXN0Pg0K')).!

plistNestedDictionary
	"
		self createPlistMethod: 'plistNestedDictionary' fromFile: 'dictionary_nested.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGljdD4NCgk8a2V5PmRhdGE8L2tleT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8
L2RhdGE+DQoJPGtleT5kYXRlPC9rZXk+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2Rh
dGU+DQoJPGtleT5mYWxzZTwva2V5Pg0KCTxmYWxzZS8+DQoJPGtleT5udW1iZXI8L2tleT4NCgk8
aW50ZWdlcj44MTU8L2ludGVnZXI+DQoJPGtleT5yZWFsPC9rZXk+DQoJPHJlYWw+My4xNDE1OTI8
L3JlYWw+DQoJPGtleT5zdHJpbmc8L2tleT4NCgk8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gg
anVtcHMgb3ZlciB0aGUgbGF6eSBkb2cncyBiYWNrPC9zdHJpbmc+DQoJPGtleT50cnVlPC9rZXk+
DQoJPHRydWUvPg0KCQ0KCQk8a2V5PmFycmF5PC9rZXk+DQoJPGFycmF5Pg0KCTxzdHJpbmc+VGhl
IHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZydzIGJhY2s8L3N0cmluZz4N
Cgk8aW50ZWdlcj44MTU8L2ludGVnZXI+DQoJPHJlYWw+My4xNDE1OTI8L3JlYWw+DQoJPHRydWUv
Pg0KCTxmYWxzZS8+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2RhdGU+DQoJPGRhdGE+
DQoJQVNORlo0bXJ6ZTg9DQoJPC9kYXRhPg0KCQ0KPGRpY3Q+DQoJPGtleT5kYXRhPC9rZXk+DQoJ
PGRhdGE+DQoJQVNORlo0bXJ6ZTg9DQoJPC9kYXRhPg0KCTxrZXk+ZGF0ZTwva2V5Pg0KCTxkYXRl
PjIwMDUtMDctMjVUMDg6Mjk6MDlaPC9kYXRlPg0KCTxrZXk+ZmFsc2U8L2tleT4NCgk8ZmFsc2Uv
Pg0KCTxrZXk+bnVtYmVyPC9rZXk+DQoJPGludGVnZXI+ODE1PC9pbnRlZ2VyPg0KCTxrZXk+cmVh
bDwva2V5Pg0KCTxyZWFsPjMuMTQxNTkyPC9yZWFsPg0KCTxrZXk+c3RyaW5nPC9rZXk+DQoJPHN0
cmluZz5UaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhlIGxhenkgZG9nJ3MgYmFjazwv
c3RyaW5nPg0KCTxrZXk+dHJ1ZTwva2V5Pg0KCTx0cnVlLz4NCgk8L2RpY3Q+DQoJPC9hcnJheT4N
CgkNCjwvZGljdD4NCjwvcGxpc3Q+DQo=')).!

plistNotAPlist
	"
		self createPlistMethod: 'plistNotAPlist' fromFile: 'nplist.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjxucGxpc3QgdmVyc2lvbj0i
MS4wIj4NCjx0cnVlLz4NCjwvbnBsaXN0Pg0K')).!

plistReal
	"
		self createPlistMethod: 'plistReal' fromFile: 'real.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8cmVhbD4zLjE0MTU5MjwvcmVhbD4NCjwvcGxpc3Q+DQo=')).!

plistString
	"
		self createPlistMethod: 'plistString' fromFile: 'string.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cn
cyBiYWNrPC9zdHJpbmc+DQo8L3BsaXN0Pg0K')).!

plistStringWithDoubleQuotes
	"
		self createPlistMethod: 'plistStringWithDoubleQuotes' fromFile: 'string_dquotes.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8c3RyaW5nPlRoZSAicXVpY2sgYnJvd24iIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRv
ZydzIGJhY2s8L3N0cmluZz4NCjwvcGxpc3Q+DQo=')).!

plistStringWithSingleQuotes
	"
		self createPlistMethod: 'plistStringWithSingleQuotes' fromFile: 'string_squotes.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8c3RyaW5nPlRoZSAncXVpY2sgYnJvd24nIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRv
ZydzIGJhY2s8L3N0cmluZz4NCjwvcGxpc3Q+DQo=')).!

plistTrue
	"
		self createPlistMethod: 'plistTrue' fromFile: 'true.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8dHJ1ZS8+DQo8L3BsaXN0Pg0K')).!

plistWrongVersion
	"
		self createPlistMethod: 'plistWrongVersion' fromFile: 'plist_v2.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjIu
MCI+DQo8dHJ1ZS8+DQo8L3BsaXN0Pg0K')).!

propertyListTypeConstant
	^NSPropertyListXMLFormat_v1_0! !
!XMLPropertyListTest class categoriesFor: #filesAndMethods!public! !
!XMLPropertyListTest class categoriesFor: #plistArray!public! !
!XMLPropertyListTest class categoriesFor: #plistData!public! !
!XMLPropertyListTest class categoriesFor: #plistDate!public! !
!XMLPropertyListTest class categoriesFor: #plistDictionary!public! !
!XMLPropertyListTest class categoriesFor: #plistFalse!public! !
!XMLPropertyListTest class categoriesFor: #plistInteger!public! !
!XMLPropertyListTest class categoriesFor: #plistNestedArray!public! !
!XMLPropertyListTest class categoriesFor: #plistNestedDictionary!public! !
!XMLPropertyListTest class categoriesFor: #plistNotAPlist!public! !
!XMLPropertyListTest class categoriesFor: #plistReal!public! !
!XMLPropertyListTest class categoriesFor: #plistString!public! !
!XMLPropertyListTest class categoriesFor: #plistStringWithDoubleQuotes!public! !
!XMLPropertyListTest class categoriesFor: #plistStringWithSingleQuotes!public! !
!XMLPropertyListTest class categoriesFor: #plistTrue!public! !
!XMLPropertyListTest class categoriesFor: #plistWrongVersion!public! !
!XMLPropertyListTest class categoriesFor: #propertyListTypeConstant!public! !

