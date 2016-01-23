/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java.functions;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.java.JavaClassStruct;
import jcl.java.JavaObjectStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public final class JNew extends FunctionStruct {

	public static final SymbolStruct J_NEW = GlobalPackageStruct.EXTENSIONS.intern("JNEW").getSymbol();

	private static final Logger LOGGER = LoggerFactory.getLogger(JNew.class);

	private JNew() {
		super("Creates a new instance of the Java Class matching the provided string", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		J_NEW.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(J_NEW);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		// TODO: we should accept a JavaClass Designator (aka. String/JavaClassObject)
		final SymbolStruct javaClassArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("JAVA-CLASS-ARG").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(javaClassArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final JavaClassStruct javaClassStruct = (JavaClassStruct) lispStructs[0];
		return jNew(javaClassStruct.getJavaClass());
	}

	public JavaObjectStruct jNew(final Class<?> javaClass) {
		final String javaClassName = javaClass.getName();
		try {
			final Constructor<?> defaultConstructor = javaClass.getDeclaredConstructor();
			final Object newInstance = defaultConstructor.newInstance();
			return new JavaObjectStruct(newInstance);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException("Java Class '" + javaClassName + "' does not have a default no argument constructor.", ex);
		} catch (final InvocationTargetException | InstantiationException | IllegalAccessException ex) {
			final String message = "Java Class '" + javaClassName + "' could not be instantiated.";
			LOGGER.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}
}
