package jcl.system.classloaders;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.FileErrorException;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoaderClassLoader extends ClassLoader {

	private static final Logger LOGGER = LoggerFactory.getLogger(LoaderClassLoader.class);

	private final Map<String, Class<?>> classes = new ConcurrentHashMap<>();

	private final JarFile jarFile;

	private final boolean verbose;

	private final boolean print;

	public LoaderClassLoader(final Path jarFilePath, final boolean verbose, final boolean print) {
		super(CompilerClassLoader.INSTANCE);
		try {
			final File file = jarFilePath.toFile();
			jarFile = new JarFile(file);
		} catch (final IOException ioe) {
			throw new ErrorException("Unable to create class loader from jar file: " + jarFilePath, ioe);
		}

		this.verbose = verbose;
		this.print = print;
	}

	public Class<?> loadMainClass() {

		final Attributes manifestMainAttributes;
		final String jarFileName = jarFile.getName();
		try {
			if (verbose) {
				LOGGER.info("; Loading {}", jarFileName);
			}

			final Manifest manifest = jarFile.getManifest();
			manifestMainAttributes = manifest.getMainAttributes();
		} catch (final IOException ioe) {
			final String msg = "Error loading compiled file: " + jarFileName;
			throw new FileErrorException(msg, ioe);
		}

		try {
			final String mainClassName = manifestMainAttributes.get(Attributes.Name.MAIN_CLASS).toString();
			loadClass(mainClassName);

			return Class.forName(mainClassName, true, this);
		} catch (final ClassNotFoundException ex) {
			throw new FileErrorException("Error loading main definition for compiled file: " + jarFileName + '\n', ex);
		}
	}

	@Override
	public Class<?> loadClass(final String name) throws ClassNotFoundException {
		final Class<?> loadedClass = loadClass(name, true);

		if (print) {
			if (loadedClass != null) {
				LOGGER.info("; {} defined", loadedClass.getSimpleName());
			}
		}
		return loadedClass;
	}

	@Override
	public Class<?> loadClass(final String name, final boolean resolve) throws ClassNotFoundException {
		if (classes.containsKey(name)) {
			return classes.get(name);
		}

		final Class<?> loadedClass = findLoadedClass(name);
		if (loadedClass != null) {
			return loadedClass;
		}

		final String fileName = name.replace('/', '.');
		final JarEntry jarEntry = jarFile.getJarEntry(fileName + ".class");
		if (jarEntry == null) {
			return internalFindClass(fileName);
		}

		Class<?> clazz = null;
		try {
			final InputStream in = jarFile.getInputStream(jarEntry);
			final byte[] bytes = IOUtils.toByteArray(in);
			clazz = defineClass(fileName, bytes, 0, bytes.length);
		} catch (final IOException ex) {
			final String jarFileName = jarFile.getName();
			LOGGER.error("Error reading the class {} from JAR file {}", fileName, jarFileName, ex);
		}

		if (resolve) {
			resolveClass(clazz);
		}

		classes.put(name, clazz);
		return clazz;
	}

	private Class<?> internalFindClass(final String className) {
		Class<?> foundClass;
		try {
			foundClass = findClass(className);
		} catch (final ClassNotFoundException ignored) {
			foundClass = internalFindSystemClass(className);
		}
		return foundClass;
	}

	private Class<?> internalFindSystemClass(final String className) {
		Class<?> foundClass = null;
		try {
			foundClass = findSystemClass(className);
		} catch (final ClassNotFoundException ignored) {
			LOGGER.warn("Unable to locate the Class: {}", className);
		}
		return foundClass;
	}
}
