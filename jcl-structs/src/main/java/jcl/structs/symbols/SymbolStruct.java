package jcl.structs.symbols;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.packages.PackageStruct;
import jcl.types.symbols.Symbol;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@code SymbolStruct} is the object representation of a Lisp 'symbol' type.
 */
public class SymbolStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	protected final String name;

	protected PackageStruct symbolPackage;
	protected TYPE value;
	protected FunctionStruct function;

	protected final Map<LispStruct, LispStruct> properties = new ConcurrentHashMap<>();

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

	public String getName() {
		return name;
	}

	public PackageStruct getSymbolPackage() {
		return symbolPackage;
	}

	public void setSymbolPackage(final PackageStruct symbolPackage) {
		this.symbolPackage = symbolPackage;
	}

	/**
	 * This method retrieves a property from the symbol internal properties.
	 *
	 * @param key the key for the property to retrieve
	 * @return the property from the symbol internal properties.
	 */
	public LispStruct getProperty(final LispStruct key) {
		return properties.get(key);
	}

	/**
	 * This method set a property in the symbol internal properties to the provided value.
	 *
	 * @param key   the key for the property to set
	 * @param value the value of the property
	 */
	public void setProperty(final LispStruct key, final LispStruct value) {
		properties.put(key, value);
	}

	public TYPE getValue() {
		return value;
	}

	public void setValue(final TYPE value) {
		this.value = value;
	}

	public FunctionStruct getFunction() {
		return function;
	}

	public void setFunction(final FunctionStruct function) {
		this.function = function;
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
			newSymbol.properties.putAll(properties);
			return newSymbol;
		} else {
			return new SymbolStruct<>(name);
		}
	}

	@Override
	public String toString() {
		return "SymbolStruct{"
				+ "name='" + name + '\''
				+ ", symbolPackage=" + symbolPackage
				+ ", properties=" + properties
				+ ", value=" + value
				+ ", function=" + function
				+ '}';
	}
}
