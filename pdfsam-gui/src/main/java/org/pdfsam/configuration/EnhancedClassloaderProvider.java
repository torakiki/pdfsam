/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/mar/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.configuration;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.pdfsam.i18n.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component providing the classloader to use in the Spring application context where jar files available in the "modules" directory have been added.
 * 
 * @author Andrea Vacondio
 *
 */
final class EnhancedClassloaderProvider {
    private static final Logger LOG = LoggerFactory.getLogger(EnhancedClassloaderProvider.class);
    private static final String MODULES_DIRECTORY = "modules";

    public static final String PDFSAM_MODULES_DIRECTORY = "org.pdfsam.modules.directory";

    private EnhancedClassloaderProvider() {
        // hide
    }

    static ClassLoader classLoader(ClassLoader classLoader) {
        requireNotNull(classLoader, "Cannot enhance null class loader");
        try {
            Path modulesPath = Optional.ofNullable(getUserSpecifiedModulesPath()).orElse(getModulesPath());
            if (!Files.isDirectory(modulesPath)) {
                LOG.info(DefaultI18nContext.getInstance().i18n("Modules directory {0} does not exist",
                        modulesPath.toString()));
                return classLoader;
            }
            LOG.debug(DefaultI18nContext.getInstance().i18n("Loading modules from {0}", modulesPath.toString()));
            try (Stream<Path> files = Files.list(modulesPath)) {
                URL[] modules = getUrls(files);
                if (modules.length > 0) {
                    LOG.trace(
                            DefaultI18nContext.getInstance().i18n("Found modules jars {0}", Arrays.toString(modules)));
                    return AccessController.doPrivileged(
                            (PrivilegedAction<URLClassLoader>) () -> new URLClassLoader(modules, classLoader));
                }
            }
        } catch (IOException | URISyntaxException ex) {
            LOG.warn(DefaultI18nContext.getInstance().i18n("Error finding modules paths"), ex);
        }
        LOG.trace(DefaultI18nContext.getInstance().i18n("No module has been found"));
        return classLoader;
    }

    private static URL[] getUrls(Stream<Path> files) throws MalformedURLException {
        Set<URI> modules = files.parallel().filter(new JarSignatureFilter()).map(Path::toUri)
                .collect(Collectors.toSet());
        // TODO remove all this cumbersome code once Eclipse stops complaining about uncaught exception when using stream.map
        List<URL> urls = new ArrayList<>();
        for (URI module : modules) {
            urls.add(module.toURL());
        }
        return urls.toArray(new URL[urls.size()]);
    }

    private static Path getModulesPath() throws URISyntaxException {
        URL jarLocation = EnhancedClassloaderProvider.class.getProtectionDomain().getCodeSource().getLocation();
        if (jarLocation != null) {
            Path jarPath = Paths.get(jarLocation.toURI());
            return Paths.get(jarPath.getParent().toString(), MODULES_DIRECTORY);
        }
        LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to find modules location."));
        return null;
    }

    private static Path getUserSpecifiedModulesPath() {
        String userPath = System.getProperty(PDFSAM_MODULES_DIRECTORY);
        if (isNotBlank(userPath)) {
            LOG.debug("User specified modules location {}", userPath);
            return Paths.get(userPath);
        }
        return null;

    }
}
