package jcl.symbols;

import jcl.packages.PackageStruct;
import jcl.structures.StructureObjectStruct;

public class DefstructSymbolStruct extends SymbolStruct<StructureObjectStruct> {

	private static final long serialVersionUID = 5591752975071847089L;

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
