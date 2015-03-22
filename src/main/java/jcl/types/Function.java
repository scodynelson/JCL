/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.String;
import java.util.List;
import java.util.Objects;

import jcl.lambdalist.variable.Key;
import jcl.lambdalist.variable.Optional;
import jcl.lambdalist.variable.Rest;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.TypeSpecifier;
import jcl.types.typespecifiers.ValuesTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link Function} is an object that represents code to be executed when an appropriate number of arguments is
 * supplied.
 * <p>
 * {@link Function} -&gt; {@link T}
 */
public interface Function extends T {

	/**
	 * Singleton instance of the {@link Function} type.
	 */
	Function INSTANCE = new Factory.FunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Function> {

		/**
		 * Gets instance of compound {@link Function} type.
		 *
		 * @param typeSpecifiers
		 * 		the required arguments
		 * @param optional
		 * 		the optional arguments
		 * @param rest
		 * 		the rest arguments
		 * @param key
		 * 		the key arguments
		 * @param valuesTypeSpecifier
		 * 		the values arguments
		 *
		 * @return the newly created compound {@link Function} type
		 */
		public static Function getInstance(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
		                                   final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
		                                   final ValuesTypeSpecifier valuesTypeSpecifier) {
			return FunctionImpl.getInstance(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
		}

		@Override
		public Function getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Function} type implementation.
		 */
		private static final class FunctionImpl extends TypeBaseClass implements Function, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -6069975817174759971L;

			/**
			 * The required arguments.
			 */
			private final List<TypeSpecifier> typeSpecifiers;

			/**
			 * The optional arguments.
			 */
			private final Optional<TypeSpecifier> optional;

			/**
			 * The rest arguments.
			 */
			private final Rest<TypeSpecifier> rest;

			/**
			 * The key arguments.
			 */
			private final Key<TypeSpecifier> key;

			/**
			 * The values arguments.
			 */
			private final ValuesTypeSpecifier valuesTypeSpecifier;

			/**
			 * Private constructor.
			 */
			private FunctionImpl() {
				this(null, null, null, null, null);
			}

			/**
			 * Private constructor for compound {@link Function} type.
			 *
			 * @param typeSpecifiers
			 * 		the required arguments
			 * @param optional
			 * 		the optional arguments
			 * @param rest
			 * 		the rest arguments
			 * @param key
			 * 		the key arguments
			 * @param valuesTypeSpecifier
			 * 		the values arguments
			 */
			private FunctionImpl(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
			                     final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
			                     final ValuesTypeSpecifier valuesTypeSpecifier) {
				super("FUNCTION");
				this.typeSpecifiers = typeSpecifiers;
				this.optional = optional;
				this.rest = rest;
				this.key = key;

				this.valuesTypeSpecifier = valuesTypeSpecifier;
			}

			/**
			 * Gets instance of compound {@link Function} type.
			 *
			 * @param typeSpecifiers
			 * 		the required arguments
			 * @param optional
			 * 		the optional arguments
			 * @param rest
			 * 		the rest arguments
			 * @param key
			 * 		the key arguments
			 * @param valuesTypeSpecifier
			 * 		the values arguments
			 *
			 * @return the newly created compound {@link Function} type
			 */
			public static Function getInstance(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
			                                   final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
			                                   final ValuesTypeSpecifier valuesTypeSpecifier) {
				return new FunctionImpl(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(typeSpecifiers)
				                            .append(optional)
				                            .append(rest)
				                            .append(key)
				                            .append(valuesTypeSpecifier)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Function)) {
					return false;
				}

				final Function functionType = (Function) obj;
				return (functionType == INSTANCE) || ((functionType instanceof FunctionImpl) && checkFunctionImplEquality((FunctionImpl) functionType));
			}

			/**
			 * This method checks the equality of the provide functionImpl object to this instance.
			 *
			 * @param functionImpl
			 * 		the functionImpl object to test for equality
			 *
			 * @return true if the functionImpl object is equivalent to this instance; false otherwise
			 */
			private boolean checkFunctionImplEquality(final FunctionImpl functionImpl) {
				return Objects.equals(typeSpecifiers, functionImpl.typeSpecifiers)
						&& Objects.equals(optional, functionImpl.optional)
						&& Objects.equals(rest, functionImpl.rest)
						&& Objects.equals(key, functionImpl.key)
						&& Objects.equals(valuesTypeSpecifier, functionImpl.valuesTypeSpecifier);
			}

			@Override
			public String toString() {
				return '(' + getName()
						+ ((typeSpecifiers == null) ? '*' : typeSpecifiers)
						+ ((optional == null) ? '*' : optional)
						+ ((rest == null) ? '*' : rest)
						+ ((key == null) ? '*' : key)
						+ ((valuesTypeSpecifier == null) ? '*' : valuesTypeSpecifier)
						+ ')';
			}
		}
	}
}
