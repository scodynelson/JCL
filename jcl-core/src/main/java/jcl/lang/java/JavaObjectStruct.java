/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class JavaObjectStruct extends BuiltInClassStruct {

	private final Object javaObject;

	private JavaObjectStruct(final Object javaObject) {
		super(null, null);

		this.javaObject = javaObject;
	}

	public Object getJavaObject() {
		return javaObject;
	}

	@Override
	public String toString() {
		return javaObject.toString();
	}

	public static JavaObjectStruct toJavaObject(final Object javaObject) {
		return new JavaObjectStruct(javaObject);
	}

	public static JavaObjectStruct toJavaObject(final Class<?> javaClass) {
		final String javaClassName = javaClass.getName();
		try {
			final Constructor<?> defaultConstructor = javaClass.getDeclaredConstructor();
			final Object newInstance = defaultConstructor.newInstance();
			return new JavaObjectStruct(newInstance);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException(
					"Java Class '" + javaClassName +
							"' does not have a default no argument constructor.", ex);
		} catch (final InvocationTargetException | InstantiationException | IllegalAccessException ex) {
			final String message = "Java Class '" + javaClassName + "' could not be instantiated.";
			log.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}
}
