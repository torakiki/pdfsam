/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.Optional;
import java.util.ResourceBundle;
import java.util.Set;

import static java.util.Objects.nonNull;
import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Context to deal with translations. It contains information about the mutable current locale and allow to translate strings to the current locale through static methods.
 *
 * @author Andrea Vacondio
 */
public final class I18nContext {

    private static final Logger LOG = LoggerFactory.getLogger(I18nContext.class);

    private final Set<Locale> supported = Set.of(Locale.of("af"), Locale.of("bs"), Locale.of("bg"), Locale.of("el"),
            Locale.of("eu"), Locale.of("pt", "BR"), Locale.SIMPLIFIED_CHINESE, Locale.TRADITIONAL_CHINESE,
            Locale.of("co"), Locale.of("ca"), Locale.of("hr"), Locale.of("cs"), Locale.of("da"), Locale.of("nl"),
            Locale.UK, Locale.FRENCH, Locale.GERMAN, Locale.of("he"), Locale.of("hi"), Locale.of("hu"), Locale.of("lv"),
            Locale.of("nb"), Locale.of("lt"), Locale.of("ms"), Locale.JAPANESE, Locale.ITALIAN, Locale.of("pl"),
            Locale.of("pt"), Locale.of("ro"), Locale.of("ru"), Locale.of("sk"), Locale.of("sl"), Locale.of("sr"),
            Locale.of("sv"), Locale.of("es"), Locale.of("tr"), Locale.of("uk"), Locale.of("fi"), Locale.of("ko"),
            Locale.of("oc"), Locale.of("lo"), Locale.of("th"));

    private final SimpleObjectProperty<Locale> locale = new SimpleObjectProperty<>();
    private Optional<ResourceBundle> bundle = empty();

    I18nContext() {
        eventStudio().addAnnotatedListeners(this);
        locale.subscribe(this::loadBundles);
    }

    @EventListener
    public void setLocale(SetLocaleRequest e) {
        if (nonNull(e.languageTag()) && !e.languageTag().isBlank()) {
            LOG.trace("Setting locale to {}", e.languageTag());
            ofNullable(Locale.forLanguageTag(e.languageTag())).filter(supported::contains).ifPresent(locale::set);
        }
    }

    private void loadBundles(Locale l) {
        if (nonNull(l)) {
            Locale.setDefault(l);
            LOG.trace("Loading i18n bundle for {}", Locale.getDefault());
            try {
                this.bundle = ofNullable(ResourceBundle.getBundle("org.pdfsam.i18n.Messages", Locale.getDefault(),
                        I18nContext.class.getModule()));
                LOG.debug("Locale set to {}", Locale.getDefault());
            } catch (Exception e) {
                LOG.error("Unable to load translations bundle", e);
            }
        }
    }

    Locale getBestLocale() {
        if (supported.contains(Locale.getDefault())) {
            LOG.trace("Using best matching locale: {}", Locale.getDefault());
            return Locale.getDefault();
        }
        var onlyLanguage = Locale.of(Locale.getDefault().getLanguage());
        if (supported.contains(onlyLanguage)) {
            LOG.trace("Using supported locale closest to default {}", onlyLanguage);
            return onlyLanguage;
        }
        LOG.trace("Using fallback locale");
        return Locale.ENGLISH;
    }

    /**
     * @return the default {@link I18nContext} instance
     */
    public static I18nContext i18n() {
        return I18nContextHolder.CONTEXT;
    }

    /**
     * @return an {@link ObservableValue} {@link Locale} representing the current locale
     */
    public ObservableValue<Locale> locale() {
        return this.locale;
    }

    public String tr(String text) {
        initBundleIfRequired();
        return bundle.filter(r -> r.containsKey(text)).map(r -> r.getString(text)).orElse(text);
    }

    /**
     * @param text    text to be translated
     * @param replace replacements for the placeholders
     * @return the translated string where {0} and {1} (etc) placeholders are replaced by the replace[0], replace[1] etc
     */
    public String tr(String text, String... replace) {
        initBundleIfRequired();
        return MessageFormat.format(tr(text), (Object[]) replace);
    }

    private void initBundleIfRequired() {
        if (bundle.isEmpty()) {
            locale.set(getBestLocale());
        }
    }

    public Set<Locale> getSupported() {
        return supported;
    }

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     *
     * @author Andrea Vacondio
     */
    private static final class I18nContextHolder {

        private I18nContextHolder() {
            // hide constructor
        }

        static final I18nContext CONTEXT = new I18nContext();
    }
}
