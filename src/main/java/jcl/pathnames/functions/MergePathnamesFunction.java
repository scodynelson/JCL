/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
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
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MergePathnamesFunction extends FunctionStruct {

	public static final SymbolStruct MERGE_PATHNAMES = GlobalPackageStruct.COMMON_LISP.intern("MERGE-PATHNAMES").getSymbol();

	private static final long serialVersionUID = 3634903325863235363L;

	@Autowired
	private PathnameFunction pathnameFunction;

	@Autowired
	private Printer printer;

	private MergePathnamesFunction() {
		super("Constructs a pathname from pathname by filling in any unsupplied components with the corresponding values from default-pathname and default-version.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MERGE_PATHNAMES.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MERGE_PATHNAMES);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct pathnameArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathnameArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalParameter> optionalBindings = new ArrayList<>(2);

		final SymbolStruct defaultPathnameArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("DEFAULT-PATHNAME").getSymbol();

		final SymbolStruct defaultPathnameSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("DEFAULT-PATHNAME-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter defaultPathnameSuppliedPBinding = new SuppliedPParameter(defaultPathnameSuppliedPSymbol);

		final OptionalParameter defaultPathnameOptionalBinding = new OptionalParameter(defaultPathnameArgSymbol, NullStruct.INSTANCE, defaultPathnameSuppliedPBinding);
		optionalBindings.add(defaultPathnameOptionalBinding);

		final SymbolStruct defaultVersionArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("DEFAULT-VERSION").getSymbol();

		final SymbolStruct defaultVersionSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("DEFAULT-VERSION-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter defaultVersionSuppliedPBinding = new SuppliedPParameter(defaultVersionSuppliedPSymbol);

		final OptionalParameter defaultVersionOptionalBinding = new OptionalParameter(defaultVersionArgSymbol, NullStruct.INSTANCE, defaultVersionSuppliedPBinding);
		optionalBindings.add(defaultVersionOptionalBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .optionalBindings(optionalBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathname = lispStructs[0];

		LispStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getValue();
		PathnameVersion defaultVersion = new PathnameVersion(PathnameVersionComponentType.NEWEST);

		final int length = lispStructs.length;
		if (length >= 1) {
			defaultPathspec = lispStructs[1];
		}
		if (length >= 2) {
			defaultVersion = getPathnameVersion(lispStructs[2]);
		}

		return mergePathnames(pathname, defaultPathspec, defaultVersion);
	}

	public PathnameStruct mergePathnames(final LispStruct pathSpec) {
		final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
		return mergePathnames(pathSpec, defaultPathspec);
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
