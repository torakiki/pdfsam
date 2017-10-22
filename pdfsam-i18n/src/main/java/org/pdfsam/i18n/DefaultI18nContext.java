/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.i18n;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.sejda.eventstudio.annotation.EventListener;
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
        supportedLocalesCache.add(new Locale("eu"));
        supportedLocalesCache.add(new Locale("bs"));
        supportedLocalesCache.add(new Locale("pt", "BR"));
        supportedLocalesCache.add(new Locale("ca"));
        supportedLocalesCache.add(Locale.SIMPLIFIED_CHINESE);
        supportedLocalesCache.add(Locale.TRADITIONAL_CHINESE);
        supportedLocalesCache.add(new Locale("co"));
        supportedLocalesCache.add(new Locale("hr"));
        supportedLocalesCache.add(new Locale("cs"));
        supportedLocalesCache.add(new Locale("da"));
        supportedLocalesCache.add(new Locale("nl"));
        supportedLocalesCache.add(Locale.UK);
        supportedLocalesCache.add(Locale.FRENCH);
        supportedLocalesCache.add(Locale.GERMAN);
        supportedLocalesCache.add(new Locale("hu"));
        supportedLocalesCache.add(new Locale("el"));
        supportedLocalesCache.add(Locale.JAPANESE);
        supportedLocalesCache.add(Locale.ITALIAN);
        supportedLocalesCache.add(new Locale("pl"));
        supportedLocalesCache.add(new Locale("pt"));
        supportedLocalesCache.add(new Locale("ro"));
        supportedLocalesCache.add(new Locale("ru"));
        supportedLocalesCache.add(new Locale("sk"));
        supportedLocalesCache.add(new Locale("es"));
        supportedLocalesCache.add(new Locale("tr"));
        supportedLocalesCache.add(new Locale("uk"));
        supportedLocalesCache.add(new Locale("fi"));

        SUPPORTED_LOCALES = Collections.unmodifiableSet(supportedLocalesCache);
    }

    private I18n i18n;

    DefaultI18nContext() {
        Locale.setDefault(getBestLocale());
        refreshBundles();
        eventStudio().addAnnotatedListeners(this);
    }

    private void refreshBundles() {
        LOG.trace("Loading i18n bundle for {}", Locale.getDefault());
        this.i18n = I18nFactory.getI18n(DefaultI18nContext.class);
        LOG.debug("Locale set to {}", Locale.getDefault().getDisplayLanguage());
    }

    @EventListener
    public void refresh(SetLocaleEvent e) {
        String localeString = e.getLocaleString();
        if (StringUtils.isNotBlank(localeString)) {
            LOG.trace("Setting default locale to {}", localeString);
            Optional.ofNullable(Locale.forLanguageTag(localeString)).filter(SUPPORTED_LOCALES::contains)
                    .ifPresent(Locale::setDefault);
            refreshBundles();
        }
    }

    Locale getBestLocale() {
        if (SUPPORTED_LOCALES.contains(Locale.getDefault())) {
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

    @Override
    public String i18n(String input) {
        return i18n.tr(input);
    }

    @Override
    public String i18n(String input, String value) {
        return i18n.tr(input, value);
    }

    @Override
    public String i18n(String input, String value0, String value1) {
        return i18n.tr(input, value0, value1);
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
