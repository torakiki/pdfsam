/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/set/2014
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
package org.pdfsam.ui.dashboard.preference;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.mockito.ArgumentCaptor;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.LocaleKeyValueItem;
import org.sejda.eventstudio.Listener;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class PreferenceAppearencePaneTest extends GuiTest {

    private UserContext userContext = mock(UserContext.class);

    @Override
    protected Parent getRootNode() {
        Locale.setDefault(Locale.ENGLISH);
        PreferenceComboBox<LocaleKeyValueItem> localeCombo = new PreferenceComboBox<>(StringUserPreference.LOCALE,
                userContext);
        localeCombo.setId("localeCombo");
        PreferenceComboBox<KeyStringValueItem<String>> startupModuleCombo = new PreferenceComboBox<>(
                StringUserPreference.STARTUP_MODULE, userContext);
        ClearStatisticsButton clearStatsButton = new ClearStatisticsButton();
        PreferenceAppearencePane victim = new PreferenceAppearencePane(localeCombo, startupModuleCombo,
                clearStatsButton);
        victim.setId("victim");
        return victim;
    }

    @Test
    public void eventSentOnLocaleChange() {
        Listener<SetLocaleEvent> listener = mock(Listener.class);
        eventStudio().add(SetLocaleEvent.class, listener);
        Locale first = DefaultI18nContext.SUPPORTED_LOCALES.stream().findFirst().get();
        click("#localeCombo").click(StringUtils.capitalize(first.getDisplayName()));
        ArgumentCaptor<SetLocaleEvent> captor = ArgumentCaptor.forClass(SetLocaleEvent.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(first.toLanguageTag(), captor.getValue().getLocaleString());
        verify(userContext).setStringPreference(eq(StringUserPreference.LOCALE), eq(first.toLanguageTag()));
    }
}
