package jcl.compiler.classloaders;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.FileErrorException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

@Log4j2
public class LoaderClassLoader extends ClassLoader {

	private final Map<String, Class<?>> classes = new ConcurrentHashMap<>();

	private final JarFile jarFile;

	private final boolean verbose;

	private final boolean print;

	public LoaderClassLoader(final File file, final boolean verbose, final boolean print) {
		super(CompilerClassLoader.INSTANCE);

		this.verbose = verbose;
		this.print = print;

		try {
			jarFile = new JarFile(file);

			final String jarFileName = jarFile.getName();
			if (verbose) {
				log.info("; Loading '{}'", jarFileName);
			}
			if (print) {
				log.info("");
			}
			loadJarEntries(jarFileName);
		} catch (final IOException ex) {
			throw new ErrorException("Unable to create class loader from jar file: " + file, ex);
		}
	}

	private void loadJarEntries(final String jarFileName) {
		final Enumeration<JarEntry> entries = jarFile.entries();
		while (entries.hasMoreElements()) {
			final JarEntry jarEntry = entries.nextElement();
			String fileName = jarEntry.getName().replace('/', '.');

			if (fileName.endsWith(".class")) {
				fileName = FilenameUtils.removeExtension(fileName);

				try {
					final InputStream in = jarFile.getInputStream(jarEntry);
					final byte[] byteArray = IOUtils.toByteArray(in);
					final Class<?> entryClass = defineClass(fileName, byteArray, 0, byteArray.length);
					resolveClass(entryClass);

					if (print) {
						log.info("; '{}' defined", fileName);
					}
					classes.put(fileName, entryClass);
				} catch (final IOException ioe) {
					log.error("Error reading the class {} from JAR file {}", fileName, jarFileName, ioe);
				}
			}
		}

		if (print) {
			log.info("");
		}
	}

	public Class<?> loadMainClass() {

		final Manifest manifest;
		final String jarFileName = jarFile.getName();
		try {
			if (verbose) {
				log.info("; Loading main class: '{}'\n", jarFileName);
			}

			manifest = jarFile.getManifest();
		} catch (final IOException ioe) {
			final String msg = "Error loading main class: " + jarFileName;
			// TODO: Take a StreamStruct!!!
			throw new FileErrorException(msg, ioe, null);
		}

		final Attributes manifestMainAttributes = manifest.getMainAttributes();
		final String mainClassName = manifestMainAttributes.get(Attributes.Name.MAIN_CLASS).toString();

		if (classes.containsKey(mainClassName)) {
			final Class<?> mainClass = classes.get(mainClassName);
			if (mainClass == null) {
				// TODO: Take a StreamStruct!!!
				throw new FileErrorException("Main class definition for compiled file was not defined correctly: " + jarFileName, null);
			}
			return mainClass;
		}

		// TODO: Take a StreamStruct!!!
		throw new FileErrorException("Error loading main class definition for compiled file: " + jarFileName, null);
	}
}
