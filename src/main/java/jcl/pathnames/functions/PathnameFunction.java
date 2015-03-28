/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameFileStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameURIStruct;
import jcl.printer.Printer;
import jcl.streams.FileStreamStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PathnameFunction extends FunctionStruct {

	public static final SymbolStruct<?> PATHNAME = new SymbolStruct<>("PATHNAME", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -353874315108380742L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PathnameFunction.class);

	@Autowired
	private Printer printer;

	private PathnameFunction() {
		super("Returns the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> pathspecArgSymbol = new SymbolStruct<>("PATHSPEC", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation pathspecArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(pathspecArgSymbol, pathspecArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
		return pathname(pathspec);
	}

	public PathnameStruct pathname(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		String namestring = "";
		try {
			if (pathnameDesignator instanceof PathnameStruct) {
				pathname = (PathnameStruct) pathnameDesignator;
			} else if (pathnameDesignator instanceof StringStruct) {
				final StringStruct namestringStruct = (StringStruct) pathnameDesignator;
				namestring = namestringStruct.getAsJavaString();
				pathname = buildPathname(namestring);
			} else if (pathnameDesignator instanceof FileStreamStruct) {
				final FileStreamStruct fileStream = (FileStreamStruct) pathnameDesignator;
				final Path path = fileStream.getPath();
				final File file = path.toFile();
				namestring = file.getAbsolutePath();
				pathname = buildPathname(namestring);
			} else {
				final String printedObject = printer.print(pathnameDesignator);
				throw new TypeErrorException("Illegal pathname designator argument provided: " + printedObject);
			}
		} catch (final URISyntaxException use) {
			final String msg = "Failed to create pathname with namestring: " + namestring;
			LOGGER.error(msg, use);
			throw new ErrorException(msg, use);
		}

		return pathname;
	}

	/**
	 * Builds and returns a pathname with the provided {@code pathname} parsed as its elements.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 *
	 * @return the constructed pathname with constructed elements
	 *
	 * @throws URISyntaxException
	 * 		if the provided pathname is determined to be a URI, but cannot be parsed as one
	 * 		NOTE: THIS SHOULD NEVER HAPPEN BUT WE THROW THIS FOR SAFETY CASES
	 */
	private static PathnameStruct buildPathname(final String pathname) throws URISyntaxException {
		if (isURI(pathname)) {
			return new PathnameURIStruct(pathname);
		} else {
			return new PathnameFileStruct(pathname);
		}
	}

	/**
	 * Determines if the provided path is a URI.
	 *
	 * @param path
	 * 		the path to test
	 *
	 * @return whether or not the provide path is a URI
	 */
	private static boolean isURI(final String path) {
		try {
			final URI uri = new URI(path);
			return uri.isAbsolute();
		} catch (final URISyntaxException use) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Provided path cannot be parsed as a URI: {}", path, use);
			}
			return false;
		}
	}

}
