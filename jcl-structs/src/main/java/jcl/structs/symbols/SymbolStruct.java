package jcl.structs.symbols;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.conses.ListStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.packages.PackageStruct;
import jcl.types.symbols.Symbol;

import java.util.List;

public class SymbolStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	private final String name;

	private PackageStruct symbolPackage;
	private List<ListStruct> propertyList;
	private TYPE value;
	private FunctionStruct function;
	private boolean isSpecial;
	private boolean isConstant;

	public SymbolStruct(final String name) {
		this(name, null, null, null, null, false, false);
	}

	public SymbolStruct(final String name, final TYPE value) {
		this(name, null, null, value, null, false, false);
	}

	public SymbolStruct(final String name, final FunctionStruct function) {
		this(name, null, null, null, function, false, false);
	}

	public SymbolStruct(final String name, final PackageStruct symbolPackage, final List<ListStruct> propertyList,
						final TYPE value, final FunctionStruct function, final boolean isSpecial, final boolean isConstant) {
		this(Symbol.INSTANCE, name, symbolPackage, propertyList, value, function, isSpecial, isConstant);
	}

	protected SymbolStruct(final Symbol symbolType,
						   final String name, final PackageStruct symbolPackage, final List<ListStruct> propertyList,
						   final TYPE value, final FunctionStruct function, final boolean isSpecial, final boolean isConstant) {
		super(symbolType, null, null);
		this.name = name;

		this.symbolPackage = symbolPackage;
		this.propertyList = propertyList;
		this.value = value;
		this.function = function;
		this.isConstant = isConstant;
		this.isSpecial = isSpecial;
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

	public List<ListStruct> getPropertyList() {
		return propertyList;
	}

	public void setPropertyList(final List<ListStruct> propertyList) {
		this.propertyList = propertyList;
	}

	public TYPE getValue() {
		return value;
	}

	public void setValue(final TYPE value) {
		if (!isConstant) {
			this.value = value;
		}
		// TODO: throw exception if constant???
	}

	public FunctionStruct getFunction() {
		return function;
	}

	public void setFunction(final FunctionStruct function) {
		this.function = function;
	}

	public boolean isSpecial() {
		return isSpecial;
	}

	public void setSpecial(final boolean special) {
		isSpecial = special;
	}

	public boolean isConstant() {
		return isConstant;
	}

	public void setConstant(final boolean constant) {
		isConstant = constant;
	}

	@Override
	public String toString() {
		return "SymbolStruct{" +
				"name='" + name + '\'' +
				", symbolPackage=" + symbolPackage +
				", propertyList=" + propertyList +
				", value=" + value +
				", function=" + function +
				", isSpecial=" + isSpecial +
				", isConstant=" + isConstant +
				'}';
	}
}
