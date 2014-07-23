package jcl.symbols;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.PackageStruct;
import jcl.types.Symbol;

import java.util.ArrayList;
import java.util.List;

/**
 * The {@code SymbolStruct} is the object representation of a Lisp 'symbol' type.
 *
 * @param <TYPE> the type of the symbol value
 */
public class SymbolStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	protected final String name;

	protected PackageStruct symbolPackage;
	protected TYPE value;
	protected FunctionStruct function;

	protected final List<LispStruct> properties = new ArrayList<>();

	// TODO: Handle special's correctly...
	protected boolean isSpecial;

	/**
	 * Public constructor.
	 *
	 * @param name the symbol name
	 */
	public SymbolStruct(final String name) {
		this(name, null, null, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name          the symbol name
	 * @param symbolPackage the symbol package
	 */
	public SymbolStruct(final String name, final PackageStruct symbolPackage) {
		this(name, symbolPackage, null, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name  the symbol name
	 * @param value the symbol value
	 */
	public SymbolStruct(final String name, final TYPE value) {
		this(name, null, value, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name     the symbol name
	 * @param function the symbol function
	 */
	public SymbolStruct(final String name, final FunctionStruct function) {
		this(name, null, null, function);
	}

	/**
	 * Public constructor.
	 *
	 * @param name          the symbol name
	 * @param symbolPackage the symbol package
	 * @param value         the symbol value
	 */
	public SymbolStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		this(Symbol.INSTANCE, name, symbolPackage, value, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param name          the symbol name
	 * @param symbolPackage the symbol package
	 * @param value         the symbol value
	 * @param function      the symbol function
	 */
	public SymbolStruct(final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		this(Symbol.INSTANCE, name, symbolPackage, value, function);
	}

	/**
	 * Protected constructor.
	 *
	 * @param symbolType    the symbol type
	 * @param name          the symbol name
	 * @param symbolPackage the symbol package
	 * @param value         the symbol value
	 * @param function      the symbol function
	 */
	protected SymbolStruct(final Symbol symbolType,
	                       final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(symbolType, null, null);
		this.name = name;

		this.symbolPackage = symbolPackage;
		this.value = value;
		this.function = function;
	}

	/**
	 * Getter for symbol name property.
	 *
	 * @return symbol name property
	 */
	public String getName() {
		return name;
	}

	/**
	 * Getter for symbol package property.
	 *
	 * @return symbol package property
	 */
	public PackageStruct getSymbolPackage() {
		return symbolPackage;
	}

	/**
	 * Setter for symbol package property.
	 *
	 * @param symbolPackage new symbol package property value
	 */
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		this.symbolPackage = symbolPackage;
	}

	/**
	 * Getter for symbol value property.
	 *
	 * @return symbol value property
	 */
	public TYPE getValue() {
		return value;
	}

	/**
	 * Setter for symbol value property.
	 *
	 * @param value new symbol value property value
	 */
	public void setValue(final TYPE value) {
		this.value = value;
	}

	/**
	 * Getter for symbol function property.
	 *
	 * @return symbol function property
	 */
	public FunctionStruct getFunction() {
		return function;
	}

	/**
	 * Setter for symbol function property.
	 *
	 * @param function new symbol function property value
	 */
	public void setFunction(final FunctionStruct function) {
		this.function = function;
	}

	/**
	 * Getter for symbol properties property.
	 *
	 * @return symbol properties property
	 */
	public List<LispStruct> getProperties() {
		return properties;
	}

	/**
	 * Getter for symbol isSpecial property.
	 *
	 * @return symbol isSpecial property
	 */
	public boolean isSpecial() {
		return isSpecial;
	}

	/**
	 * This method retrieves a property from the symbol internal properties.
	 *
	 * @param key the key for the property to retrieve
	 * @return the property from the symbol internal properties.
	 */
	public LispStruct getProperty(final LispStruct key) {
		for (int i = 0; i < properties.size(); i += 2) {
			final LispStruct current = properties.get(i);
			if (key.equals(current)) {
				final int valueIndex = i + 1;
				return properties.get(valueIndex);
			}
		}
		return null;
	}

	/**
	 * This method set a property in the symbol internal properties to the provided value.
	 *
	 * @param key   the key for the property to set
	 * @param value the value of the property
	 */
	public void setProperty(final LispStruct key, final LispStruct value) {
		for (int i = 0; i < properties.size(); i += 2) {
			final LispStruct current = properties.get(i);
			if (key.equals(current)) {
				final int valueIndex = i + 1;
				properties.remove(valueIndex);
				properties.add(valueIndex, value);
			}
		}
	}

	/**
	 * This method copies the symbol and possibly it's properties.
	 *
	 * @param copyProperties whether or not to copy the symbol's internal properties
	 * @return the newly copied symbol
	 */
	public SymbolStruct<TYPE> copySymbol(final boolean copyProperties) {
		if (copyProperties) {
			final SymbolStruct<TYPE> newSymbol = new SymbolStruct<>(name, symbolPackage, value, function);
			newSymbol.properties.addAll(properties);
			return newSymbol;
		} else {
			return new SymbolStruct<>(name);
		}
	}

	@Override
	public String printStruct() {
		// TODO: Fix *PRINT-ESCAPE* typing
		final SymbolStruct<?> printEscape = (SymbolStruct<?>) Variable.PRINT_ESCAPE.getValue();


		if (!printEscape.equals(NILStruct.INSTANCE)) {
			return name; // TODO: deal with *PRINT-CASE*
		} else {
			return toString();
		}
	}

	@Override
	public String toString() {
		return "SymbolStruct{"
				+ "name='" + name + '\''
				+ ", symbolPackage=" + symbolPackage
				+ ", value=" + value
				+ ", function=" + function
				+ ", properties=" + properties
				+ ", isSpecial=" + isSpecial
				+ '}';
	}
}
