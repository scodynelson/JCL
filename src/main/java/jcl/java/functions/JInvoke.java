/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java.functions;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.java.JavaMethodStruct;
import jcl.java.JavaObjectStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class JInvoke extends FunctionStruct {

	public static final SymbolStruct J_INVOKE = GlobalPackageStruct.EXTENSIONS.intern("JINVOKE").getSymbol();

	private static final long serialVersionUID = 5787148469982523185L;

	private static final Logger LOGGER = LoggerFactory.getLogger(JInvoke.class);

	private JInvoke() {
		super("Creates a new instance of the Java Class matching the provided string", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		J_INVOKE.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(J_INVOKE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct javaMethodArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("JAVA-METHOD-ARG").getSymbol();
		final RequiredParameter javaMethodArgRequiredBinding = new RequiredParameter(javaMethodArgSymbol);
		requiredBindings.add(javaMethodArgRequiredBinding);

		final SymbolStruct javaObjectArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("JAVA-OBJECT-ARG").getSymbol();
		final RequiredParameter javaObjectArgRequiredBinding = new RequiredParameter(javaObjectArgSymbol);
		requiredBindings.add(javaObjectArgRequiredBinding);

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

		final JavaMethodStruct javaMethodStruct = (JavaMethodStruct) lispStructsAsList.get(0);
		final Method javaMethod = javaMethodStruct.getJavaMethod();

		final JavaObjectStruct javaObjectStruct = (JavaObjectStruct) lispStructsAsList.get(1);
		final Object javaObject = javaObjectStruct.getJavaObject();

		final List<LispStruct> args = lispStructsAsList.subList(2, lispStructsAsList.size());
		final LispStruct[] methodArgs = new LispStruct[args.size()];
		for (int i = 0; i < args.size(); i++) {
			final LispStruct currentArg = args.get(i);
			methodArgs[i] = currentArg;
		}

		return jInvoke(javaMethod, javaObject, methodArgs);
	}

	public LispStruct jInvoke(final Method javaMethod, final Object javaObject, final LispStruct... methodArgs) {

		final String javaMethodName = javaMethod.getName();

		final Class<?> javaObjectClass = javaObject.getClass();
		final String javaObjectClassName = javaObjectClass.getName();
		try {
			final Object methodResult = javaMethod.invoke(javaObject, (Object[]) methodArgs);
			if (methodResult instanceof LispStruct) {
				return (LispStruct) methodResult;
			}
			return new JavaObjectStruct(methodResult);
		} catch (final InvocationTargetException | IllegalAccessException ex) {
			final String message = "Java Method '" + javaMethodName + "' could not be properly invoked on Java Class '" + javaObjectClassName + "'.";
			LOGGER.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}
}
