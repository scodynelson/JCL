/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.java.JavaClassStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class JClass extends FunctionStruct {

	public static final SymbolStruct J_CLASS = GlobalPackageStruct.EXTENSIONS.intern("JCLASS").getSymbol();

	private static final long serialVersionUID = -4901948171672174993L;

	private JClass() {
		super("Gets the Java class matching the provided class name string.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		J_CLASS.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(J_CLASS);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct classNameArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("CLASS-NAME-ARG").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(classNameArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final StringStruct className = (StringStruct) lispStructs[0];
		return jClass(className.getAsJavaString());
	}

	public JavaClassStruct jClass(final String className) {
		try {
			final Class<?> javaClass = Class.forName(className);
			return new JavaClassStruct(javaClass);
		} catch (final ClassNotFoundException ex) {
			throw new ErrorException("Java Class not found for class name '" + className + "'.", ex);
		}
	}
}
