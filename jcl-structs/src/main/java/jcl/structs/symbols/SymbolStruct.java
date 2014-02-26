package jcl.structs.symbols;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.packages.PackageStruct;
import jcl.types.symbols.Symbol;

import java.util.HashMap;
import java.util.Map;

public class SymbolStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	private final String name;

	private PackageStruct symbolPackage;
	private TYPE value;
	private FunctionStruct function;

	private final Map<LispStruct, LispStruct> properties = new HashMap<>();

	public SymbolStruct(final String name) {
		this(name, null, null, null);
	}

	public SymbolStruct(final String name, final PackageStruct symbolPackage) {
		this(name, symbolPackage, null, null);
	}

	public SymbolStruct(final String name, final TYPE value) {
		this(name, null, value, null);
	}

	public SymbolStruct(final String name, final FunctionStruct function) {
		this(name, null, null, function);
	}

	public SymbolStruct(final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		this(Symbol.INSTANCE, name, symbolPackage, value, function);
	}

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

	public LispStruct getProperty(final LispStruct key) {
		return properties.get(key);
	}

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
		return "SymbolStruct{" +
				"name='" + name + '\'' +
				", symbolPackage=" + symbolPackage +
				", properties=" + properties +
				", value=" + value +
				", function=" + function +
				'}';
	}
}
