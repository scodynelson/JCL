/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java.functions;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.java.JavaClassStruct;
import jcl.java.JavaMethodStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class JMethod extends FunctionStruct {

	public static final SymbolStruct J_METHOD = GlobalPackageStruct.EXTENSIONS.intern("JMETHOD").getSymbol();

	private static final long serialVersionUID = -1933229031095497469L;

	private JMethod() {
		super("Gets the Java method matching the provided method name string for the provided Java Class object and the provided Java Class parameter object types.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		J_METHOD.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(J_METHOD);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct methodNameArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("METHOD-NAME-ARG").getSymbol();
		final RequiredParameter methodNameArgRequiredBinding = new RequiredParameter(methodNameArgSymbol);
		requiredBindings.add(methodNameArgRequiredBinding);

		// TODO: we should accept a JavaClass Designator (aka. String/JavaClassObject)
		final SymbolStruct javaClassArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("JAVA-CLASS-ARG").getSymbol();
		final RequiredParameter javaClassArgRequiredBinding = new RequiredParameter(javaClassArgSymbol);
		requiredBindings.add(javaClassArgRequiredBinding);

		final SymbolStruct objectRestArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECTS").getSymbol();
		final RestParameter restBinding = new RestParameter(objectRestArgSymbol);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .restBinding(restBinding)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final List<LispStruct> lispStructsAsList = Arrays.asList(lispStructs);

		final StringStruct methodName = (StringStruct) lispStructsAsList.get(0);
		final String methodNameString = methodName.getAsJavaString();

		final JavaClassStruct javaClassStruct = (JavaClassStruct) lispStructsAsList.get(1);
		final Class<?> javaClass = javaClassStruct.getJavaClass();

		final List<LispStruct> args = lispStructsAsList.subList(2, lispStructsAsList.size());
		final Class<?>[] parameterTypes = new Class<?>[args.size()];
		for (int i = 0; i < args.size(); i++) {
			final LispStruct currentArg = args.get(i);
			final JavaClassStruct methodParamClassStruct = (JavaClassStruct) currentArg;
			parameterTypes[i] = methodParamClassStruct.getJavaClass();
		}

		return jMethod(methodNameString, javaClass, parameterTypes);
	}

	public JavaMethodStruct jMethod(final String methodName, final Class<?> javaClass, final Class<?>... parameterTypes) {

		final String javaClassName = javaClass.getName();
		try {
			final Method method = javaClass.getDeclaredMethod(methodName, parameterTypes);
			return new JavaMethodStruct(method);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException("Java Class '" + javaClassName + "' does not have the method '" + methodName + "' with parameter types '" + Arrays.toString(parameterTypes) + "'.", ex);
		}
	}
}
