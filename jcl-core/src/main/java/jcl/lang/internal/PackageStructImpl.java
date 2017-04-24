/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.KeywordStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.PackageErrorException;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.PackageType;

/**
 * The {@link PackageStructImpl} is the object representation of a Lisp 'package' type.
 */
public class PackageStructImpl extends BuiltInClassStruct implements PackageStruct {

	public static final KeywordStruct INTERNAL_KEYWORD = KeywordStructImpl.valueOf("INTERNAL");
	public static final KeywordStruct EXTERNAL_KEYWORD = KeywordStructImpl.valueOf("EXTERNAL");
	public static final KeywordStruct INHERITED_KEYWORD = KeywordStructImpl.valueOf("INHERITED");

	/**
	 * The {@link List} of {@link PackageStruct}s that the package uses.
	 */
	private final List<PackageStruct> useList = new ArrayList<>();

	/**
	 * The {@link List} of {@link PackageStruct}s that the package is used by.
	 */
	private final List<PackageStruct> usedByList = new ArrayList<>();

	/**
	 * The {@link Map} of the packages external {@link SymbolStruct}s.
	 * NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols (aka. anything in ExternalSymbols or
	 * ShadowingSymbols are in InternalSymbols but not vice-versa)
	 */
	protected final Map<String, SymbolStruct> externalSymbols = new ConcurrentHashMap<>();

	/**
	 * The {@link Map} of the packages internal {@link SymbolStruct}s.
	 */
	private final Map<String, SymbolStruct> internalSymbols = new ConcurrentHashMap<>();

	/**
	 * The {@link Map} of the packages shadowing {@link SymbolStruct}s.
	 * NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols (aka. anything in ExternalSymbols or
	 * ShadowingSymbols are in InternalSymbols but not vice-versa)
	 */
	private final Map<String, SymbolStruct> shadowingSymbols = new ConcurrentHashMap<>();

	/**
	 * The name of package.
	 */
	private String name;

	/**
	 * The {@link List} of nicknames of the package.
	 */
	private List<String> nicknames;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 */
	protected PackageStructImpl(final String name) {
		this(name, new ArrayList<>());
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 * @param nicknames
	 * 		the package nicknames
	 */
	private PackageStructImpl(final String name, final List<String> nicknames) {
		this(name, nicknames, new ArrayList<>());
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 * @param nicknames
	 * 		the package nicknames
	 * @param useList
	 * 		the packages this package will use/inherit from
	 */
	private PackageStructImpl(final String name, final List<String> nicknames, final PackageStruct... useList) {
		this(name, nicknames, new ArrayList<>(Arrays.asList(useList)));
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 * @param nicknames
	 * 		the package nicknames
	 * @param useList
	 * 		the packages this package will use/inherit from
	 */
	private PackageStructImpl(final String name, final List<String> nicknames, final List<PackageStruct> useList) {
		super(PackageType.INSTANCE, null, null);
		this.name = name;
		this.nicknames = nicknames;

		this.useList.addAll(useList);
		final PackageStruct[] useListArray = new PackageStruct[useList.size()];
		internalUsePackage(useList.toArray(useListArray));

		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		GlobalPackageStruct.ALL_PACKAGES.put(name, this);
		for (final String nickname : nicknames) {
			GlobalPackageStruct.ALL_PACKAGES.put(nickname, this);
		}
	}

	public static PackageStruct valueOf(final String name) {
		return new PackageStructImpl(name);
	}

	public static PackageStruct valueOf(final String name, final List<String> nicknames) {
		return new PackageStructImpl(name, nicknames);
	}

	public static PackageStruct valueOf(final String name, final List<String> nicknames, final PackageStruct... useList) {
		return new PackageStructImpl(name, nicknames, useList);
	}

	public static PackageStruct valueOf(final String name, final List<String> nicknames, final List<PackageStruct> useList) {
		return new PackageStructImpl(name, nicknames, useList);
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public List<String> getNicknames() {
		return nicknames;
	}

	@Override
	public Map<String, SymbolStruct> getExternalSymbols() {
		return externalSymbols;
	}

	@Override
	public Map<String, SymbolStruct> getShadowingSymbols() {
		return shadowingSymbols;
	}

	@Override
	public List<PackageStruct> getUseList() {
		return new ArrayList<>(useList);
	}

	@Override
	public List<PackageStruct> getUsedByList() {
		return new ArrayList<>(usedByList);
	}

	@Override
	public void renamePackage(final String newName) {
		renamePackage(newName, Collections.emptyList());
	}

	@Override
	public void renamePackage(final String newName, final List<String> newNicknames) {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		name = newName;
		GlobalPackageStruct.ALL_PACKAGES.put(newName, this);

		nicknames.forEach(GlobalPackageStruct.ALL_PACKAGES::remove);
		nicknames = newNicknames;
		for (final String nickname : newNicknames) {
			GlobalPackageStruct.ALL_PACKAGES.put(nickname, this);
		}
	}

	@Override
	public void deletePackage() {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		nicknames.forEach(GlobalPackageStruct.ALL_PACKAGES::remove);
		for (final PackageStruct usedByPackage : usedByList) {
			usedByPackage.unUsePackage(this);
		}
		usedByList.clear();
		useList.clear();
		name = null;
	}

	@Override
	public void usePackage(final PackageStruct... packagesToUse) {
		useList.addAll(Arrays.asList(packagesToUse));
		internalUsePackage(packagesToUse);
	}

	/**
	 * Internal implementation of 'use-package' for updating the package to use the provided {@code packagesToUse}.
	 *
	 * @param packagesToUse
	 * 		the packages that will be used
	 */
	private void internalUsePackage(final PackageStruct... packagesToUse) {
		for (final PackageStruct packageToUse : packagesToUse) {
			if (packageToUse.eq(GlobalPackageStruct.KEYWORD)) {
				throw new PackageErrorException(this + " can't use package " + GlobalPackageStruct.KEYWORD, this);
			}

			// Shadow current nonInherited symbols
			for (final String symbolName : packageToUse.getExternalSymbols().keySet()) {
				final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
				if (nonInheritedPackageSymbol != null) {
					final SymbolStruct nonInheritedSymbol = nonInheritedPackageSymbol.getSymbol();
					shadowingSymbols.put(symbolName, nonInheritedSymbol);
				}
			}

			packageToUse.getUsedByList().add(this);
		}
	}

	@Override
	public void unUsePackage(final PackageStruct... packagesToUnUse) {
		useList.removeAll(Arrays.asList(packagesToUnUse));
		for (final PackageStruct packageToUnUse : packagesToUnUse) {
			// NOTE: We will leave the shadows in the shadowing list. This is due to the fact that we would have to search
			// through ALL used packages to make sure that there aren't any other inherited symbols that the symbol names
			// are shadowing. That's just overkill, when keeping them in the shadowing list won't affect anything.
			packageToUnUse.getUsedByList().remove(this);
		}
	}

	@Override
	public PackageSymbolStruct findSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		final PackageSymbolStruct foundPackageSymbol = findNonInheritedSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct foundSymbol = findInheritedSymbol(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, INHERITED_KEYWORD);
		}

		return null;
	}

	@Override
	public void importSymbols(final SymbolStruct... symbols) {
		for (final SymbolStruct symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol != null) {
				continue;
			}

			internalSymbols.put(symbolName, symbol);

			final SymbolStruct foundSymbol = findInheritedSymbol(symbolName);
			if (foundSymbol != null) {
				shadowingSymbols.put(symbolName, symbol);
			}

			if (symbol.getSymbolPackage() == null) {
				symbol.setSymbolPackage(this);
			}
		}
	}

	@Override
	public void shadowingImport(final SymbolStruct... symbols) {
		for (final SymbolStruct symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol != null) {
				final SymbolStruct nonInheritedSymbol = nonInheritedPackageSymbol.getSymbol();
				unintern(nonInheritedSymbol);
			}

			internalSymbols.put(symbolName, symbol);
			shadowingSymbols.put(symbolName, symbol);

			if (symbol.getSymbolPackage() == null) {
				symbol.setSymbolPackage(this);
			}
		}
	}

	@Override
	public void export(final SymbolStruct... symbols) {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStruct symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
			if (foundPackageSymbol == null) {
				notFoundSymbolNames.add(symbolName);
				continue;
			}

			if (externalSymbols.containsKey(symbolName)) {
				continue; // go to next symbol. already external
			}

			importSymbols(symbol); // This will put the symbol in the "InternalSymbols" and possibly "ShadowingSymbols"
			externalSymbols.put(symbolName, symbol);

			// NOTE: We CAN do this it seems, but we're not required to. Should we though???
//			for (final PackageStruct usedByPackage : usedByList) {
//				usedByPackage.inheritedSymbols.put(symbolName, symbol);
//			}
		}

		handleExportSymbolsNotInPackageError(notFoundSymbolNames);
	}

	@Override
	public void unexport(final SymbolStruct... symbols) {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStruct symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
			if (foundPackageSymbol == null) {
				notFoundSymbolNames.add(symbolName);
				continue;
			}

			externalSymbols.remove(symbolName);
		}

		handleExportSymbolsNotInPackageError(notFoundSymbolNames);
	}

	/**
	 * Handles the building of the {@link PackageErrorException} when symbols are not accessible in the package and
	 * cannot be exported or unexported.
	 *
	 * @param notFoundSymbolNames
	 * 		the symbols not accessible in the package
	 */
	private void handleExportSymbolsNotInPackageError(final List<String> notFoundSymbolNames) {
		if (!notFoundSymbolNames.isEmpty()) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("The following symbols are not accessible in package " + this + ": (");
			for (final String notFoundSymbolName : notFoundSymbolNames) {
				exceptionStringBuilder.append(notFoundSymbolName);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString(), this);
		}
	}

	@Override
	public void shadow(final String... symbolNames) {
		for (final String symbolName : symbolNames) {
			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);

			final SymbolStruct nonInheritedSymbol;
			if (nonInheritedPackageSymbol == null) {
				nonInheritedSymbol = SymbolStructImpl.valueOf(symbolName);
				internalSymbols.put(symbolName, nonInheritedSymbol);
				nonInheritedSymbol.setSymbolPackage(this);
			} else {
				nonInheritedSymbol = nonInheritedPackageSymbol.getSymbol();
			}
			shadowingSymbols.put(symbolName, nonInheritedSymbol);
		}
	}

	@Override
	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct symbolStruct = SymbolStructImpl.valueOf(symbolName);
		internalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, INTERNAL_KEYWORD);
	}

	@Override
	public boolean unintern(final SymbolStruct symbol) {
		final String symbolName = symbol.getName();

		// Test for conflicts BEFORE we remove anything
		final Set<SymbolStruct> shadowingConflicts = getShadowingConflicts(symbolName);
		if (shadowingConflicts.size() > 1) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("Uninterning " + symbolName + " from " + this + " would cause conflicts among : (");
			for (final SymbolStruct conflictingSymbol : shadowingConflicts) {
				exceptionStringBuilder.append(conflictingSymbol);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString(), this);
		}

		final SymbolStruct externalSymbol = externalSymbols.remove(symbolName);
		final SymbolStruct shadowingSymbol = shadowingSymbols.remove(symbolName);
		final SymbolStruct internalSymbol = internalSymbols.remove(symbolName);

		symbol.setSymbolPackage(null);

		return (externalSymbol != null) || (shadowingSymbol != null) || (internalSymbol != null);
	}

	/**
	 * Determines if a name conflict exists with the symbolName and that it is currently resolved due to a shadowing
	 * symbol existence.
	 *
	 * @param symbolName
	 * 		the name of the symbol to check for shadowing conflicts
	 *
	 * @return the conflicting symbols if any exist, or null if no conflicts exist
	 */
	private Set<SymbolStruct> getShadowingConflicts(final String symbolName) {
		if (!shadowingSymbols.containsKey(symbolName)) {
			return Collections.emptySet();
		}

		final Set<SymbolStruct> conflictingInheritedSymbols = new HashSet<>();
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol != null) {
				final SymbolStruct inheritedSymbol = inheritedPackageSymbol.getSymbol();
				conflictingInheritedSymbols.add(inheritedSymbol);
			}
		}
		return conflictingInheritedSymbols;
	}

	/**
	 * Locates the non-inherited symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found and it's package location type, or null if not found
	 */
	private PackageSymbolStruct findNonInheritedSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		SymbolStruct foundSymbol = externalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, EXTERNAL_KEYWORD);
		}

		foundSymbol = shadowingSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, INTERNAL_KEYWORD);
		}

		foundSymbol = internalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, INTERNAL_KEYWORD);
		}

		return null;
	}

	/**
	 * Locates the inherited symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found, or null if not found
	 */
	private SymbolStruct findInheritedSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		SymbolStruct foundSymbol = null;
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol == null) {
				continue;
			}

			final KeywordStruct packageSymbolType = inheritedPackageSymbol.getPackageSymbolType();
			if (EXTERNAL_KEYWORD.eq(packageSymbolType)) {
				foundSymbol = inheritedPackageSymbol.getSymbol();
				break;
			}
		}

		return foundSymbol;
	}

	@Override
	public String toString() {
//		final String typeClassName = getType().getClass().getSimpleName().toUpperCase();
		return "#<PACKAGE \"" + name + "\">";
	}
}
