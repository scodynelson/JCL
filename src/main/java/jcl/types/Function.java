package jcl.types;

import jcl.lambdalist.variable.Key;
import jcl.lambdalist.variable.Optional;
import jcl.lambdalist.variable.Rest;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.TypeSpecifier;
import jcl.typespecifiers.ValuesTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;
import java.util.List;
import java.util.Objects;

/**
 * A {@link Function} is an object that represents code to be executed when an appropriate number of arguments is
 * supplied.
 * <p>
 * {@link Function} -> {@link T}
 */
public interface Function extends T {

	Function INSTANCE = new Factory.FunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Function> {

		@Override
		public Function getInstance() {
			return INSTANCE;
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
			return FunctionImpl.getInstance(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
		}

		/**
		 * Inner {@link Function} type implementation.
		 */
		private static class FunctionImpl extends TypeBaseClass implements Function, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final List<TypeSpecifier> typeSpecifiers;
			private final Optional<TypeSpecifier> optional;
			private final Rest<TypeSpecifier> rest;
			private final Key<TypeSpecifier> key;

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
				super("FUNCTION", GlobalPackageStruct.COMMON_LISP);
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
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
