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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class I18NContextTest {

    private I18nContext victim;

    @BeforeEach
    public void before() {
        this.victim = new I18nContext();
    }

    @AfterEach
    public void after() {
        eventStudio().clear();
    }

    @Test
    public void setLocaleSupported() {
        eventStudio().broadcast(new SetLocaleRequest("it"));
        assertEquals(Locale.ITALIAN, victim.locale().getValue());

    }

    @Test
    public void getBestLocaleSupported() {
        Locale.setDefault(Locale.ITALIAN);
        assertNull(victim.locale().getValue());
        victim.tr("chuck norris");
        assertEquals(Locale.ITALIAN, victim.locale().getValue());
    }

    @Test
    public void getBestLocaleSupportedLanguage() {
        Locale.setDefault(Locale.of("en", "CA"));
        assertNull(victim.locale().getValue());
        victim.tr("chuck norris");
        assertEquals(Locale.ENGLISH, victim.locale().getValue());
    }

    @Test
    public void getBestLocaleNotSupportedLanguage() {
        Locale.setDefault(Locale.of("mn", "MN"));
        assertNull(victim.locale().getValue());
        victim.tr("chuck norris");
        assertEquals(Locale.ENGLISH, victim.locale().getValue());
    }
}
