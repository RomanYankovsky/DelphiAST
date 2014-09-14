{---------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwSimplePasParTypes, released November 14, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
unit CastaliaPasLexTypes;

----------------------------------------------------------------------------}

unit SimpleParser.Types;

interface

uses SysUtils, TypInfo;

type
  TmwParseError = (
    InvalidAdditiveOperator,
    InvalidAccessSpecifier,
    InvalidCharString,
    InvalidClassMethodHeading,
    InvalidConstantDeclaration,
    InvalidConstSection,
    InvalidDeclarationSection,
    InvalidDirective16Bit,
    InvalidDirectiveBinding,
    InvalidDirectiveCalling,
    InvalidExportedHeading,
    InvalidForStatement,
    InvalidInitializationSection,
    InvalidInterfaceDeclaration,
    InvalidInterfaceType,
    InvalidLabelId,
    InvalidLabeledStatement,
    InvalidMethodHeading,
    InvalidMultiplicativeOperator,
    InvalidNumber,
    InvalidOrdinalIdentifier,
    InvalidParameter,
    InvalidParseFile,
    InvalidProceduralDirective,
    InvalidProceduralType,
    InvalidProcedureDeclarationSection,
    InvalidProcedureMethodDeclaration,
    InvalidRealIdentifier,
    InvalidRelativeOperator,
    InvalidStorageSpecifier,
    InvalidStringIdentifier,
    InvalidStructuredType,
    InvalidTryStatement,
    InvalidTypeKind,
    InvalidVariantIdentifier,
    InvalidVarSection,
    vchInvalidClass, //vch
    vchInvalidMethod, //vch
    vchInvalidProcedure,//vch
    vchInvalidCircuit, //vch
    vchInvalidIncludeFile //vch
    );

  TmwPasCodeInfo = (
    ciNone,           //: @BUG heresy  !
    ciAccessSpecifier,
    ciAdditiveOperator,
    ciArrayConstant,
    ciArrayType,
    ciAsmStatement,
    ciBlock,
    ciCaseLabel,
    ciCaseSelector,
    ciCaseStatement,
    ciCharString,
    ciClassClass,           //DR 2001-07-16
    ciClassField,
    ciClassForward,
    ciClassFunctionHeading,
    ciClassHeritage,
    ciClassMemberList,
    ciClassMethodDirective,
    ciClassMethodHeading,
    ciClassMethodOrProperty,
    ciClassMethodResolution,
    ciClassProcedureHeading,
    ciClassProperty,
    ciClassReferenceType,
    ciClassType,
    ciClassTypeEnd,			// DR 2001-07-31
    ciClassVisibility,
    ciCompoundStatement,
	ciConstantColon,
    ciConstantDeclaration,
    ciConstantEqual,
    ciConstantExpression,
    ciConstantName,
    ciConstantValue,
    ciConstantValueTyped,
    ciConstParameter,
    ciConstructorHeading,
    ciConstructorName,
    ciConstSection,
    ciContainsClause,
    ciContainsExpression,
    ciContainsIdentifier,
    ciContainsStatement,
    ciDeclarationSection,
    ciDesignator,
    ciDestructorHeading,
    ciDestructorName,
    ciDirective16Bit,
    ciDirectiveBinding,
	ciDirectiveCalling,
	ciDirectiveDeprecated,	// DR 2001-10-20
	ciDirectiveLibrary,		// DR 2001-10-20
	ciDirectiveLocal,		// DR 2001-11-14
	ciDirectivePlatform,	// DR 2001-10-20
	ciDirectiveVarargs,		// DR 2001-11-14	
	ciDispIDSpecifier,		// DR 2001-07-26
    ciDispInterfaceForward,
    ciEmptyStatement,
	ciEnumeratedType,
	ciEnumeratedTypeItem,	// DR 2001-10-29
    ciExceptBlock,
    ciExceptionBlockElseBranch,
    ciExceptionClassTypeIdentifier,
    ciExceptionHandler,
    ciExceptionHandlerList,
    ciExceptionIdentifier,
    ciExceptionVariable,
    ciExpliciteType,
    ciExportedHeading,
    ciExportsClause,
    ciExportsElement,
    ciExpression,
    ciExpressionList,
    ciExternalDirective,
    ciExternalDirectiveThree,
    ciExternalDirectiveTwo,
    ciFactor,
    ciFieldDeclaration,
    ciFieldList,
    ciFileType,
    ciFormalParameterList,
    ciFormalParameterSection,
    ciForStatement,
	ciForwardDeclaration, // DR 2001-07-23
    ciFunctionHeading,
    ciFunctionMethodDeclaration,
    ciFunctionMethodName,
	ciFunctionProcedureBlock,
	ciFunctionProcedureName,
	ciHandlePtCompDirect,   //XM 20001125
	ciHandlePtDefineDirect, //XM 20001125
	ciHandlePtElseDirect,   //XM 20001125
	ciHandlePtIfDefDirect,  //XM 20001125
	ciHandlePtEndIfDirect,  //XM 20001125
	ciHandlePtIfNDefDirect, //XM 20001125
	ciHandlePtIfOptDirect,  //XM 20001125
	ciHandlePtIncludeDirect,//XM 20001125
	ciHandlePtResourceDirect,//XM 20001125
	ciHandlePtUndefDirect, //XM 20001125
	ciIdentifier,
    ciIdentifierList,
    ciIfStatement,
    ciImplementationSection,
    ciIncludeFile,
    ciIndexSpecifier,	// DR 2001-07-26
    ciInheritedStatement,
    ciInitializationSection,
    ciInlineStatement,
    ciInterfaceDeclaration,
    ciInterfaceForward,
    ciInterfaceGUID,
    ciInterfaceHeritage,
    ciInterfaceMemberList,
    ciInterfaceSection,
    ciInterfaceType,
    ciLabelDeclarationSection,
    ciLabeledStatement,
	ciLabelId,
	ciLibraryFile,
    ciMainUsedUnitExpression,
    ciMainUsedUnitName,
    ciMainUsedUnitStatement,
    ciMainUsesClause,
    ciMultiplicativeOperator,
    ciNewFormalParameterType,
    ciNumber,
    ciNextToken, //XM 20002512
    ciObjectConstructorHeading,
    ciObjectDestructorHeading,
    ciObjectField,
    ciObjectForward,
    ciObjectFunctionHeading,
    ciObjectHeritage,
    ciObjectMemberList,
    ciObjectMethodDirective,
    ciObjectMethodHeading,
    ciObjectNameOfMethod,
    ciObjectProcedureHeading,
    ciObjectProperty,          	// DR 2001-08-07
    ciObjectPropertySpecifiers, // DR 2001-08-07
    ciObjectType,
    ciObjectTypeEnd,			// DR 2001-08-07
    ciObjectVisibility,
    ciOldFormalParameterType,
    ciOrdinalIdentifier,
    ciOrdinalType,
    ciOutParameter,
	ciPackageFile,
	ciParameterFormal,
    ciParameterName,
    ciParameterNameList,
    ciParseFile,
    ciPointerType,
    ciProceduralDirective,
    ciProceduralType,
    ciProcedureDeclarationSection,
    ciProcedureHeading,
    ciProcedureMethodDeclaration,
    ciProcedureMethodName,
    ciProgramBlock,
    ciProgramFile,
    ciPropertyDefault,		// DR 2001-07-16
    ciPropertyInterface,
    ciPropertyName,
    ciPropertyParameterConst,
    ciPropertyParameterList,
    ciPropertySpecifiers,
    ciQualifiedIdentifier,
    ciQualifiedIdentifierList,
    ciRaiseStatement,
    ciReadAccessIdentifier,
    ciRealIdentifier,
    ciRealType,
    ciRecordConstant,
    ciRecordFieldConstant,
    ciRecordType,
    ciRecordVariant,
	ciRelativeOperator,
    ciRepeatStatement,
    ciRequiresClause,
    ciRequiresIdentifier,
    ciResolutionInterfaceName,
    ciResourceDeclaration,
    ciReturnType,
    ciSEMICOLON, //XM 20002512
    ciSetConstructor,
    ciSetElement,
    ciSetType,
    ciSimpleExpression,
    ciSimpleStatement,
    ciSimpleType,
    ciSkipAnsiComment,
    ciSkipBorComment,
    ciSkipSlashesComment,
    ciSkipSpace, //XM 20002511
    ciSkipCRLFco, //XM 20002511
    ciSkipCRLF, //XM 20002511
    ciStatement,
    ciStatementList,
    ciStorageExpression,
    ciStorageIdentifier,
    ciStorageDefault,
    ciStorageNoDefault,
    ciStorageSpecifier,
    ciStorageStored,
    ciStringIdentifier,
	ciStringStatement,
    ciStringType,
    ciStructuredType,
    ciSubrangeType,
    ciTagField,
    ciTagFieldName,
    ciTagFieldTypeName,
    ciTerm,
    ciTryStatement,
    ciTypedConstant,
    ciTypeDeclaration,
    ciTypeId,
    ciTypeKind,
    ciTypeName,
    ciTypeSection,
    ciUnitFile,
    ciUnitId,
    ciUsedUnitName,
    ciUsedUnitsList,
    ciUsesClause,
    ciVarAbsolute,
    ciVarEqual,
    ciVarDeclaration,
    ciVariable,
    ciVariableList,
    ciVariableReference,
    ciVariableTwo,
    ciVariantIdentifier,
    ciVariantSection,
	ciVarParameter,
    ciVarSection,
    ciVisibilityAutomated,
    ciVisibilityPrivate,
    ciVisibilityProtected,
    ciVisibilityPublic,
    ciVisibilityPublished,
    ciVisibilityUnknown,
    ciWhileStatement,
    ciWithStatement,
    ciWriteAccessIdentifier
    );

function ParserErrorName(Value: TmwParseError): string;

implementation

function ParserErrorName(Value: TmwParseError): string;
begin
  result := GetEnumName(TypeInfo(TmwParseError), Integer(Value));
end;

end.

