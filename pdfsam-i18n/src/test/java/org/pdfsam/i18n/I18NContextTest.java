/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2014
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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.Locale;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class I18NContextTest {

    @AfterEach
    public void tearDown() {
        eventStudio().clear();
    }

    @Test
    public void setInvalidLanguageTag() {
        Locale.setDefault(Locale.UK);
        var observer = new I18nContext().locale().test();
        eventStudio().broadcast(new SetLocaleRequest("Chuck Norris"));
        observer.assertNoValues();
    }

    @Test
    public void setLocaleSupported() {
        var observer = new I18nContext().locale().test();
        eventStudio().broadcast(new SetLocaleRequest("it"));
        observer.assertValue(Locale.ITALIAN);
    }

    @Test
    public void getBestLocaleSupported() {
        var victim = new I18nContext();
        Locale.setDefault(Locale.ITALIAN);
        var observer = victim.locale().test();
        observer.assertNoValues();
        victim.tr("chuck norris");
        observer.assertValue(Locale.ITALIAN);
    }

    @Test
    public void getBestLocaleSupportedLanguage() {
        var victim = new I18nContext();
        Locale.setDefault(Locale.of("en", "CA"));
        var observer = victim.locale().test();
        observer.assertNoValues();
        victim.tr("chuck norris");
        observer.assertValue(Locale.ENGLISH);
    }

    @Test
    public void getBestLocaleNotSupportedLanguage() {
        var victim = new I18nContext();
        Locale.setDefault(Locale.CANADA_FRENCH);
        var observer = victim.locale().test();
        observer.assertNoValues();
        victim.tr("chuck norris");
        observer.assertValue(Locale.FRENCH);
    }
}
