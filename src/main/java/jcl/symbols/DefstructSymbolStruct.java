package jcl.symbols;

import jcl.classes.StructureObjectStruct;
import jcl.packages.PackageStruct;

public class DefstructSymbolStruct extends SymbolStruct<StructureObjectStruct> {

	private String javaName;

	public DefstructSymbolStruct(final String name, final StructureObjectStruct value) {
		super(name, value);
		javaName = "Defstruct" + System.currentTimeMillis();
	}

	public DefstructSymbolStruct(final String name, final PackageStruct symbolPackage, final StructureObjectStruct value) {
		super(name, symbolPackage, value, null);
		javaName = "Defstruct" + System.currentTimeMillis();
	}

	public String getJavaName() {
		return javaName;
	}

	public void setJavaName(final String javaName) {
		this.javaName = javaName;
	}
}
