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
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameDevice;
import jcl.pathnames.PathnameDirectory;
import jcl.pathnames.PathnameFileStruct;
import jcl.pathnames.PathnameHost;
import jcl.pathnames.PathnameName;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.PathnameURIStruct;
import jcl.pathnames.PathnameVariables;
import jcl.pathnames.PathnameVersion;
import jcl.pathnames.PathnameVersionComponentType;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MergePathnamesFunction extends FunctionStruct {

	public static final SymbolStruct<?> MERGE_PATHNAMES = new SymbolStruct<>("MERGE-PATHNAMES", GlobalPackageStruct.COMMON_LISP);

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
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> pathnameArgSymbol = new SymbolStruct<>("PATHNAME", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation pathnameArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(pathnameArgSymbol, pathnameArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = new ArrayList<>(2);

		final SymbolStruct<?> defaultPathnameArgSymbol = new SymbolStruct<>("DEFAULT-PATHNAME", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation defaultPathnameArgAllocation = new ParameterAllocation(1);

		final SymbolStruct<?> defaultPathnameSuppliedPSymbol = new SymbolStruct<>("DEFAULT-PATHNAME-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation defaultPathnameSuppliedPAllocation = new ParameterAllocation(2);
		final SuppliedPBinding defaultPathnameSuppliedPBinding = new SuppliedPBinding(defaultPathnameSuppliedPSymbol, defaultPathnameSuppliedPAllocation);

		final OptionalBinding defaultPathnameOptionalBinding = new OptionalBinding(defaultPathnameArgSymbol, defaultPathnameArgAllocation, NullStruct.INSTANCE, defaultPathnameSuppliedPBinding);
		optionalBindings.add(defaultPathnameOptionalBinding);

		final SymbolStruct<?> defaultVersionArgSymbol = new SymbolStruct<>("DEFAULT-VERSION", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation defaultVersionArgAllocation = new ParameterAllocation(3);

		final SymbolStruct<?> defaultVersionSuppliedPSymbol = new SymbolStruct<>("DEFAULT-VERSION-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation defaultVersionSuppliedPAllocation = new ParameterAllocation(4);
		final SuppliedPBinding defaultVersionSuppliedPBinding = new SuppliedPBinding(defaultVersionSuppliedPSymbol, defaultVersionSuppliedPAllocation);

		final OptionalBinding defaultVersionOptionalBinding = new OptionalBinding(defaultVersionArgSymbol, defaultVersionArgAllocation, NullStruct.INSTANCE, defaultVersionSuppliedPBinding);
		optionalBindings.add(defaultVersionOptionalBinding);

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathname = lispStructs[0];

		final int length = lispStructs.length;
		if (length > 1) {
			final LispStruct defaultPathspec = lispStructs[1];
			if (length > 2) {
				final PathnameVersion defaultVersion = getPathnameVersion(lispStructs[2]);
				return mergePathnames(pathname, defaultPathspec, defaultVersion);
			} else {
				return mergePathnames(pathname, defaultPathspec);
			}
		} else {
			return mergePathnames(pathname);
		}
	}

	public PathnameStruct mergePathnames(final LispStruct pathSpec) {
		final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getValue();
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
		if (mergedPathnameName == null) {
			mergedPathnameName = defaultPathname.getPathnameName();
		}
		if (mergedPathnameType == null) {
			mergedPathnameType = defaultPathname.getPathnameType();
		}
		if (mergedPathnameVersion == null) {
			mergedPathnameVersion = defaultPathname.getPathnameVersion();
		} else if (defaultVersion.getComponentType() != PathnameVersionComponentType.NIL) {
			mergedPathnameVersion = defaultVersion;
		}

		if (pathname instanceof PathnameFileStruct) {
			return new PathnameFileStruct(mergedPathnameHost, mergedPathnameDevice, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
		} else if (pathname instanceof PathnameURIStruct) {
			return new PathnameURIStruct(mergedPathnameHost, mergedPathnameDevice, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
		} else {
			return new LogicalPathnameStruct(mergedPathnameHost, mergedPathnameDirectory, mergedPathnameName, mergedPathnameType, mergedPathnameVersion);
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
