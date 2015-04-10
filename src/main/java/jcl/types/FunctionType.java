/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

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
 * A {@link FunctionType} is an object that represents code to be executed when an appropriate number of arguments is
 * supplied.
 * <p>
 * {@link FunctionType} -&gt; {@link TType}
 */
public interface FunctionType extends TType {

	/**
	 * Singleton instance of the {@link FunctionType} type.
	 */
	FunctionType INSTANCE = new Factory.FunctionTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<FunctionType> {

		/**
		 * Gets instance of compound {@link FunctionType} type.
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
		 * @return the newly created compound {@link FunctionType} type
		 */
		public static FunctionType getInstance(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
		                                       final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
		                                       final ValuesTypeSpecifier valuesTypeSpecifier) {
			return FunctionTypeImpl.getInstance(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
		}

		@Override
		public FunctionType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link FunctionType} type implementation.
		 */
		private static final class FunctionTypeImpl extends TypeBaseClass implements FunctionType, AtomicTypeSpecifier, CompoundTypeSpecifier {

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
			private FunctionTypeImpl() {
				this(null, null, null, null, null);
			}

			/**
			 * Private constructor for compound {@link FunctionType} type.
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
			private FunctionTypeImpl(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
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
			 * Gets instance of compound {@link FunctionType} type.
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
			 * @return the newly created compound {@link FunctionType} type
			 */
			public static FunctionType getInstance(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
			                                       final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
			                                       final ValuesTypeSpecifier valuesTypeSpecifier) {
				return new FunctionTypeImpl(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
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

				if (!(obj instanceof FunctionType)) {
					return false;
				}

				final FunctionType functionType = (FunctionType) obj;
				return (functionType == INSTANCE) || ((functionType instanceof FunctionTypeImpl) && checkFunctionImplEquality((FunctionTypeImpl) functionType));
			}

			/**
			 * This method checks the equality of the provided {@code functionTypeImpl} object to this instance.
			 *
			 * @param functionTypeImpl
			 * 		the FunctionTypeImpl object to test for equality
			 *
			 * @return true if the {@code functionTypeImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkFunctionImplEquality(final FunctionTypeImpl functionTypeImpl) {
				return Objects.equals(typeSpecifiers, functionTypeImpl.typeSpecifiers)
						&& Objects.equals(optional, functionTypeImpl.optional)
						&& Objects.equals(rest, functionTypeImpl.rest)
						&& Objects.equals(key, functionTypeImpl.key)
						&& Objects.equals(valuesTypeSpecifier, functionTypeImpl.valuesTypeSpecifier);
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
