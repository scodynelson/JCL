/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.io.File;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameStruct;
import jcl.printer.Printer;
import jcl.streams.FileStreamStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameFunction extends FunctionStruct {

	public static final SymbolStruct<?> PATHNAME = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME").getSymbol();

	private static final long serialVersionUID = -353874315108380742L;

	@Autowired
	private Printer printer;

	private PathnameFunction() {
		super("Returns the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(PATHNAME);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathspecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
		return pathname(pathspec);
	}

	public PathnameStruct pathname(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		final String namestring;
		if (pathnameDesignator instanceof PathnameStruct) {
			pathname = (PathnameStruct) pathnameDesignator;
		} else if (pathnameDesignator instanceof StringStruct) {
			final StringStruct namestringStruct = (StringStruct) pathnameDesignator;
			namestring = namestringStruct.getAsJavaString();
			pathname = new PathnameStruct(namestring);
		} else if (pathnameDesignator instanceof FileStreamStruct) {
			final FileStreamStruct fileStream = (FileStreamStruct) pathnameDesignator;
			final Path path = fileStream.getPath();
			final File file = path.toFile();
			namestring = file.getAbsolutePath();
			pathname = new PathnameStruct(namestring);
		} else {
			final String printedObject = printer.print(pathnameDesignator);
			throw new TypeErrorException("Illegal pathname designator argument provided: " + printedObject);
		}

		return pathname;
	}
}
