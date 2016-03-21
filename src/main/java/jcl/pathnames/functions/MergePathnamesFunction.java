/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameDevice;
import jcl.pathnames.PathnameDirectory;
import jcl.pathnames.PathnameHost;
import jcl.pathnames.PathnameName;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.PathnameVariables;
import jcl.pathnames.PathnameVersion;
import jcl.pathnames.PathnameVersionComponentType;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MergePathnamesFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MERGE-PATHNAMES";
	private static final String PATHNAME_ARGUMENT = "PATHNAME";
	private static final String DEFAULT_PATHNAME_ARGUMENT = "DEFAULT-PATHNAME";
	private static final String DEFAULT_VERSION_ARGUMENT = "DEFAULT-VERSION";

	@Autowired
	private PathnameFunction pathnameFunction;

	@Autowired
	private Printer printer;

	public MergePathnamesFunction() {
		super("Constructs a pathname from pathname by filling in any unsupplied components with the corresponding values from default-pathname and default-version.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHNAME_ARGUMENT)
		                .optionalParameter(DEFAULT_PATHNAME_ARGUMENT).withInitialValue(PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getValue())
		                .optionalParameter(DEFAULT_VERSION_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct pathname = arguments.getRequiredArgument(PATHNAME_ARGUMENT);

		LispStruct defaultPathspec = arguments.getOptionalArgument(DEFAULT_PATHNAME_ARGUMENT);
		PathnameVersion defaultVersion = new PathnameVersion(PathnameVersionComponentType.NEWEST);

		if (arguments.hasOptionalArgument(DEFAULT_VERSION_ARGUMENT)) {
			defaultVersion = getPathnameVersion(arguments.getOptionalArgument(DEFAULT_VERSION_ARGUMENT));
		}

		return mergePathnames(pathname, defaultPathspec, defaultVersion);
	}

	public PathnameStruct mergePathnames(final LispStruct pathSpec, final LispStruct defaultPathspec) {
		final PathnameVersion defaultVersion = new PathnameVersion(PathnameVersionComponentType.NEWEST);
		return mergePathnames(pathSpec, defaultPathspec, defaultVersion);
	}

	public PathnameStruct mergePathnames(final LispStruct pathSpec, final LispStruct defaultPathspec,
	                                     final PathnameVersion defaultVersion) {

		final PathnameStruct pathname = pathnameFunction.pathname(pathSpec);
		final PathnameStruct defaultPathname = pathnameFunction.pathname(defaultPathspec);

		PathnameHost mergedPathnameHost = pathname.getPathnameHost();
		PathnameDevice mergedPathnameDevice = pathname.getPathnameDevice();
		PathnameDirectory mergedPathnameDirectory = pathname.getPathnameDirectory();
		PathnameName mergedPathnameName = pathname.getPathnameName();
		PathnameType mergedPathnameType = pathname.getPathnameType();
		PathnameVersion mergedPathnameVersion = pathname.getPathnameVersion();

		if (mergedPathnameHost == null) {
			mergedPathnameHost = defaultPathname.getPathnameHost();
		}
		if (mergedPathnameDevice == null) {
			mergedPathnameDevice = defaultPathname.getPathnameDevice();
		}
		if (mergedPathnameDirectory == null) {
			mergedPathnameDirectory = defaultPathname.getPathnameDirectory();
		}
		if ((mergedPathnameName == null) || (mergedPathnameName.getComponentType() == PathnameComponentType.NIL)) {
			mergedPathnameName = defaultPathname.getPathnameName();
		}
		if ((mergedPathnameType == null) || (mergedPathnameType.getComponentType() == PathnameComponentType.NIL)) {
			mergedPathnameType = defaultPathname.getPathnameType();
		}
		if (mergedPathnameVersion == null) {
			mergedPathnameVersion = defaultPathname.getPathnameVersion();
		} else if (defaultVersion.getComponentType() != PathnameVersionComponentType.NIL) {
			mergedPathnameVersion = defaultVersion;
		}

		if (pathname instanceof LogicalPathnameStruct) {
			return new LogicalPathnameStruct(mergedPathnameHost, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
		} else {
			return new PathnameStruct(mergedPathnameHost, mergedPathnameDevice, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
		}
	}

	private PathnameVersion getPathnameVersion(final LispStruct defaultVersion) {

		final PathnameVersionComponentType componentType = PathnameVersionComponentType.fromValue(defaultVersion);
		if (componentType == null) {
			if (defaultVersion instanceof IntegerStruct) {
				final IntegerStruct integer = (IntegerStruct) defaultVersion;
				final BigInteger bigInteger = integer.getBigInteger();
				if (bigInteger.compareTo(BigInteger.ZERO) < 0) {
					final String printedInteger = printer.print(integer);
					throw new ErrorException("Integer versions must be non-negative. Got: " + printedInteger);
				}
				return new PathnameVersion(bigInteger.intValueExact());
			} else {
				final String printedDefaultVersion = printer.print(defaultVersion);
				throw new ErrorException("Pathname versions must be either a non-negative integer, :WILD, :NEWEST, :UNSPECIFIC, or NIL. Got: " + printedDefaultVersion);
			}
		}

		return new PathnameVersion(componentType);
	}
}
