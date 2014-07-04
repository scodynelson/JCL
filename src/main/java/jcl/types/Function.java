package jcl.types;

import jcl.lambdalist.variable.Key;
import jcl.lambdalist.variable.Optional;
import jcl.lambdalist.variable.Rest;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.TypeSpecifier;
import jcl.typespecifiers.ValuesTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.util.List;
import java.util.Objects;

/**
 * A {@code Function} is an object that represents code to be executed when an appropriate number of arguments is supplied.
 * <p>
 * {@code Function} -> {@code T}
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
		 * Gets instance of compound {@code Function} type.
		 *
		 * @param typeSpecifiers      the required arguments
		 * @param optional            the optional arguments
		 * @param rest                the rest arguments
		 * @param key                 the key arguments
		 * @param valuesTypeSpecifier the values arguments
		 * @return the newly created compound {@code Function} type
		 */
		public static Function getInstance(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
										   final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
										   final ValuesTypeSpecifier valuesTypeSpecifier) {
			return FunctionImpl.getInstance(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
		}

		/**
		 * Inner {@code Function} type implementation.
		 */
		private static class FunctionImpl implements Function, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final List<TypeSpecifier> typeSpecifiers;
			private final Optional<TypeSpecifier> optional;
			private final Rest<TypeSpecifier> rest;
			private final Key<TypeSpecifier> key;

			private final ValuesTypeSpecifier valuesTypeSpecifier;

			/**
			 * Private constructor.
			 */
			private FunctionImpl() {
				typeSpecifiers = null;
				optional = null;
				rest = null;
				key = null;

				valuesTypeSpecifier = null;
			}

			/**
			 * Private constructor for compound {@code Cons} type.
			 *
			 * @param typeSpecifiers      the required arguments
			 * @param optional            the optional arguments
			 * @param rest                the rest arguments
			 * @param key                 the key arguments
			 * @param valuesTypeSpecifier the values arguments
			 */
			private FunctionImpl(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
								 final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
								 final ValuesTypeSpecifier valuesTypeSpecifier) {
				this.typeSpecifiers = typeSpecifiers;
				this.optional = optional;
				this.rest = rest;
				this.key = key;

				this.valuesTypeSpecifier = valuesTypeSpecifier;
			}

			/**
			 * Gets instance of compound {@code Function} type.
			 *
			 * @param typeSpecifiers      the required arguments
			 * @param optional            the optional arguments
			 * @param rest                the rest arguments
			 * @param key                 the key arguments
			 * @param valuesTypeSpecifier the values arguments
			 * @return the newly created compound {@code Function} type
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

				final Function function = (Function) obj;
				if (function == INSTANCE) {
					return true;
				}

				return (function instanceof FunctionImpl) && checkFunctionImplEquality((FunctionImpl) function);
			}

			/**
			 * This method checks the equality of the provide functionImpl object to this instance.
			 *
			 * @param functionImpl the functionImpl object to test for equality
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
				return new HashCodeBuilder()
						.append(typeSpecifiers)
						.append(optional)
						.append(rest)
						.append(key)
						.append(valuesTypeSpecifier)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "FunctionImpl{"
						+ "typeSpecifiers=" + typeSpecifiers
						+ ", optional=" + optional
						+ ", rest=" + rest
						+ ", key=" + key
						+ ", valuesTypeSpecifier=" + valuesTypeSpecifier
						+ '}';
			}
		}
	}
}