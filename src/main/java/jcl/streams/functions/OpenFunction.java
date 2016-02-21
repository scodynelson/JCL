/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameStruct;
import jcl.printer.Printer;
import jcl.streams.DirectionType;
import jcl.streams.ExternalFormat;
import jcl.streams.FileStreamStructs;
import jcl.streams.IfDoesNotExistType;
import jcl.streams.IfExistsType;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.CharacterType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class OpenFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public OpenFunction() {
		super("Writes the characters of the sub-sequence of string bounded by start and end to output-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "FILESPEC").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Create the list of {@link KeyParameter}s for the bounding of the string to output.
	 *
	 * @return a list of {@link KeyParameter}s
	 */
	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyParameters = new ArrayList<>(5);
		final KeyParameter direction
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "DIRECTION")
				              .initForm(IntegerStruct.ZERO)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(direction);
		final KeyParameter elementType
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "ELEMENT-TYPE")
				              .initForm(NILStruct.INSTANCE)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(elementType);
		final KeyParameter ifExists
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "IF-EXISTS")
				              .initForm(NILStruct.INSTANCE)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(ifExists);
		final KeyParameter ifDoesNotExist
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "IF-DOES-NOT-EXIST")
				              .initForm(NILStruct.INSTANCE)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(ifDoesNotExist);
		final KeyParameter externalFormat
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "EXTERNAL_KEYWORD-FORMAT")
				              .initForm(NILStruct.INSTANCE)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(externalFormat);
		return keyParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final PathnameStruct pathname = lispStructs[0].toPathname().get();

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, 1, CommonLispSymbols.DIRECTION_KEYWORD, CommonLispSymbols.ELEMENT_TYPE_KEYWORD,
				              CommonLispSymbols.IF_EXISTS_KEYWORD, CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD,
				              CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);

		final LispStruct directionParam
				= keywords.getOrDefault(CommonLispSymbols.START_KEYWORD, CommonLispSymbols.INPUT_KEYWORD);
		final SymbolStruct directionSymbol =
				validator.validateType(directionParam, functionName(), "Direction", SymbolType.INSTANCE, SymbolStruct.class);
		final DirectionType directionType = DirectionType.fromValue(directionSymbol);

		final LispStruct elementTypeParam
				= keywords.getOrDefault(CommonLispSymbols.ELEMENT_TYPE_KEYWORD, CommonLispSymbols.DEFAULT_KEYWORD);
		validator.validateTypes(elementTypeParam, functionName(), "Element Type", SymbolType.INSTANCE);

		if (!(elementTypeParam instanceof LispType)) {
			final String printedObject = printer.print(elementTypeParam);
			throw new TypeErrorException(functionName() + ": Element Type must be an actual TYPE. Got: " + printedObject);
		}
		final LispType elementType = (LispType) elementTypeParam;

		final LispStruct ifExistsParam
				= keywords.getOrDefault(CommonLispSymbols.IF_EXISTS_KEYWORD, CharacterType.INSTANCE);
		final SymbolStruct ifExistsSymbol =
				validator.validateType(ifExistsParam, functionName(), "If Exists", SymbolType.INSTANCE, SymbolStruct.class);
		final IfExistsType ifExistsType = IfExistsType.fromValue(ifExistsSymbol);

		final LispStruct ifDoesNotExistParam
				= keywords.getOrDefault(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, CharacterType.INSTANCE);
		final SymbolStruct ifDoesNotExistSymbol =
				validator.validateType(ifDoesNotExistParam, functionName(), "If Does Not Exist", SymbolType.INSTANCE, SymbolStruct.class);
		final IfDoesNotExistType ifDoesNotExistType = IfDoesNotExistType.fromValue(ifDoesNotExistSymbol);

		final LispStruct externalFormatParam
				= keywords.getOrDefault(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD, CharacterType.INSTANCE);
		if (!(externalFormatParam instanceof ExternalFormat)) {
			final String printedObject = printer.print(externalFormatParam);
			throw new TypeErrorException(functionName() + ": External Format must be an actual EXTERNAL-FORMAT. Got: " + printedObject);
		}
		final ExternalFormat externalFormat = (ExternalFormat) externalFormatParam;

		return FileStreamStructs.open(pathname, directionType, elementType, ifExistsType, ifDoesNotExistType, externalFormat);
	}

	@Override
	protected String functionName() {
		return "OPEN";
	}
}
