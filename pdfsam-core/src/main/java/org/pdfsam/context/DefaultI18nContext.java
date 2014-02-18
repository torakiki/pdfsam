/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.context;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xnap.commons.i18n.I18n;
import org.xnap.commons.i18n.I18nFactory;

/**
 * Default implementation of the {@link I18nContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class DefaultI18nContext implements I18nContext {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultI18nContext.class);
    public static final Set<Locale> SUPPORTED_LOCALES;
    static {
        Set<Locale> supportedLocalesCache = new LinkedHashSet<>();
        supportedLocalesCache.add(new Locale("ar"));
        supportedLocalesCache.add(new Locale("ast"));
        supportedLocalesCache.add(new Locale("bs"));
        supportedLocalesCache.add(new Locale("pt", "BR"));
        supportedLocalesCache.add(new Locale("bg"));
        supportedLocalesCache.add(new Locale("ca"));
        supportedLocalesCache.add(new Locale("hr"));
        supportedLocalesCache.add(new Locale("cs"));
        supportedLocalesCache.add(new Locale("da"));
        supportedLocalesCache.add(new Locale("nl"));
        supportedLocalesCache.add(Locale.UK);
        supportedLocalesCache.add(new Locale("fa"));
        supportedLocalesCache.add(new Locale("et"));
        supportedLocalesCache.add(new Locale("fi"));
        supportedLocalesCache.add(Locale.FRENCH);
        supportedLocalesCache.add(new Locale("gl"));
        supportedLocalesCache.add(Locale.GERMAN);
        supportedLocalesCache.add(new Locale("el"));
        supportedLocalesCache.add(new Locale("iw", "IL"));
        supportedLocalesCache.add(new Locale("hu"));
        supportedLocalesCache.add(Locale.JAPANESE);
        supportedLocalesCache.add(new Locale("id"));
        supportedLocalesCache.add(Locale.ITALIAN);
        supportedLocalesCache.add(Locale.KOREAN);
        supportedLocalesCache.add(new Locale("nb"));
        supportedLocalesCache.add(new Locale("lv"));
        supportedLocalesCache.add(new Locale("lt"));
        supportedLocalesCache.add(new Locale("pl"));
        supportedLocalesCache.add(new Locale("pt"));
        supportedLocalesCache.add(new Locale("ro"));
        supportedLocalesCache.add(new Locale("ru"));
        supportedLocalesCache.add(Locale.SIMPLIFIED_CHINESE);
        supportedLocalesCache.add(new Locale("sk"));
        supportedLocalesCache.add(new Locale("sl"));
        supportedLocalesCache.add(new Locale("es"));
        supportedLocalesCache.add(new Locale("sv"));
        supportedLocalesCache.add(new Locale("tr"));
        supportedLocalesCache.add(new Locale("th"));
        supportedLocalesCache.add(new Locale("uk"));
        supportedLocalesCache.add(new Locale("vi"));
        supportedLocalesCache.add(Locale.TRADITIONAL_CHINESE);
        supportedLocalesCache.add(new Locale("zh", "HK"));
        SUPPORTED_LOCALES = Collections.unmodifiableSet(supportedLocalesCache);
    }

    private I18n i18n;

    private DefaultI18nContext() {
        Locale locale = getBestLocale();
        LOG.trace("Loading i18n bundle for {}", locale);
        Locale.setDefault(locale);
        this.i18n = I18nFactory.getI18n(DefaultI18nContext.class, locale);
        LOG.debug("Locale set to {}", locale.getDisplayLanguage());
    }

    public Locale getLocale() {
        return Locale.getDefault();
    }

    private Locale getBestLocale() {
        String localeString = DefaultUserContext.getInstance().getLocale();
        if (StringUtils.isNotBlank(localeString)) {
            LOG.trace("Found locale string {}", localeString);
            return Locale.forLanguageTag(localeString);
        }
        if (SUPPORTED_LOCALES.contains(Locale.getDefault())) {
            LOG.trace("Using default locale {}", Locale.getDefault());
            return Locale.getDefault();
        }
        Locale onlyLanguage = new Locale(Locale.getDefault().getLanguage());
        if (SUPPORTED_LOCALES.contains(onlyLanguage)) {
            LOG.trace("Using supported locale closest to default {}", onlyLanguage);
            return onlyLanguage;
        }
        LOG.trace("Using fallback locale");
        return Locale.UK;
    }

    /**
     * @return the default {@link I18nContext} instance
     */
    public static I18nContext getInstance() {
        return DefaultI18nContextHolder.CONTEXT;
    }

    public String i18n(String input, String... values) {
        return i18n.tr(input, values);
    }

    public ResourceBundle getResources() {
        return i18n.getResources();
    }

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class DefaultI18nContextHolder {

        private DefaultI18nContextHolder() {
            // hide constructor
        }

        static final DefaultI18nContext CONTEXT = new DefaultI18nContext();
    }
}
