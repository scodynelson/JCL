/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameStruct;
import jcl.printer.Printer;
import jcl.streams.DirectionType;
import jcl.streams.FileStreamStruct;
import jcl.streams.IfDoesNotExistType;
import jcl.streams.IfExistsType;
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.BooleanType;
import jcl.types.CharacterType;
import jcl.types.IntegerType;
import jcl.types.KeywordType;
import jcl.types.NILType;
import jcl.types.StreamType;
import jcl.types.StringType;
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

		final LispStruct pathnameDesignator = lispStructs[0];

		final PathnameStruct pathname = pathnameDesignator.toPathname().get();
		final File filePathnameFile = new File(pathname.getNamestring());
		final Path filePath = filePathnameFile.toPath();

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, 1, CommonLispSymbols.DIRECTION_KEYWORD, CommonLispSymbols.ELEMENT_TYPE_KEYWORD,
				CommonLispSymbols.IF_EXISTS_KEYWORD, CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD,
				CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);

		final LispStruct directionParam
				= keywords.getOrDefault(CommonLispSymbols.START_KEYWORD, CommonLispSymbols.INPUT_KEYWORD);
		validator.validateTypes(directionParam, functionName(), "Direction", SymbolType.INSTANCE);
		final SymbolStruct directionSymbol = (SymbolStruct) directionParam;
		final DirectionType directionType = DirectionType.fromValue(directionSymbol);

		final LispStruct elementTypeParam
				= keywords.getOrDefault(CommonLispSymbols.ELEMENT_TYPE_KEYWORD, CommonLispSymbols.DEFAULT_KEYWORD);
		validator.validateTypes(elementTypeParam, functionName(), "Element Type", SymbolType.INSTANCE);

		if (!(elementTypeParam instanceof LispType)) {
			final String printedObject = printer.print(elementTypeParam);
			throw new TypeErrorException(functionName() + ": Element Type must be an actual TYPE. Got: " + printedObject);
		}

		final LispStruct ifExistsParam
				= keywords.getOrDefault(CommonLispSymbols.IF_EXISTS_KEYWORD, CharacterType.INSTANCE);
		validator.validateTypes(ifExistsParam, functionName(), "If Exists", SymbolType.INSTANCE);
		final SymbolStruct ifExistsSymbol = (SymbolStruct) ifExistsParam;
		final IfExistsType ifExistsType = IfExistsType.fromValue(ifExistsSymbol);

		final LispStruct ifDoesNotExistParam
				= keywords.getOrDefault(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, CharacterType.INSTANCE);
		validator.validateTypes(ifDoesNotExistParam, functionName(), "If Does Not Exist", SymbolType.INSTANCE);
		final SymbolStruct ifDoesNotExistSymbol = (SymbolStruct) ifDoesNotExistParam;
		final IfDoesNotExistType ifDoesNotExistType = IfDoesNotExistType.fromValue(ifDoesNotExistSymbol);

		final LispStruct externalFormatParam
				= keywords.getOrDefault(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD, CharacterType.INSTANCE);
//		validator.validateTypes(externalFormatParam, functionName(), "Element Type", SymbolType.INSTANCE);

		return new FileStreamStruct(filePath);
	}

	@Override
	protected String functionName() {
		return "OPEN";
	}
}
