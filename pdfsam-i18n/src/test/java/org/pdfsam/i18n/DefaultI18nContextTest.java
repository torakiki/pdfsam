/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.junit.Assert.assertEquals;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import org.junit.After;
import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultI18nContextTest {

    @After
    public void tearDown() {
        eventStudio().clear();
    }

    @Test
    public void refreshSets() {
        DefaultI18nContext victim = new DefaultI18nContext();
        Locale.setDefault(Locale.UK);
        victim.refresh(new SetLocaleEvent(Locale.ITALIAN.toLanguageTag()));
        assertEquals(Locale.ITALIAN, Locale.getDefault());
    }

    @Test
    public void refreshNotExistingDoesntExplode() {
        DefaultI18nContext victim = new DefaultI18nContext();
        Locale.setDefault(Locale.UK);
        victim.refresh(new SetLocaleEvent("Chuck Norris"));
        assertEquals(Locale.UK, Locale.getDefault());
    }

    @Test
    public void getBestLocaleNotSupported() {
        DefaultI18nContext victim = new DefaultI18nContext();
        Locale.setDefault(Locale.CANADA);
        assertEquals(Locale.UK, victim.getBestLocale());
    }

    @Test
    public void getBestLocaletSupported() {
        DefaultI18nContext victim = new DefaultI18nContext();
        Locale.setDefault(Locale.ITALIAN);
        assertEquals(Locale.ITALIAN, victim.getBestLocale());
    }

    @Test
    public void getBestLocaletLanguageSupported() {
        DefaultI18nContext victim = new DefaultI18nContext();
        Locale.setDefault(Locale.CANADA_FRENCH);
        assertEquals(Locale.FRENCH, victim.getBestLocale());
    }
}
