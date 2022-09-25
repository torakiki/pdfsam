/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/set/2014
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
package org.pdfsam.ui.dashboard.preference;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.mockito.ArgumentCaptor;
import org.pdfsam.NoHeadless;
import org.pdfsam.core.context.StringUserPreference;
import org.pdfsam.core.context.UserContext;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.core.support.KeyStringValueItem;
import org.pdfsam.core.support.LocaleKeyValueItem;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferenceAppearencePaneTest extends ApplicationTest {

    private UserContext userContext = mock(UserContext.class);

    @Override
    public void start(Stage stage) {
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
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Category(NoHeadless.class)
    public void eventSentOnLocaleChange() {
        Listener<SetLocaleRequest> listener = mock(Listener.class);
        eventStudio().add(SetLocaleRequest.class, listener);
        PreferenceComboBox<LocaleKeyValueItem> localeCombo = lookup("#localeCombo").queryAs(PreferenceComboBox.class);
        LocaleKeyValueItem first = localeCombo.getItems().get(0);
        clickOn("#localeCombo").sleep(1000).clickOn(StringUtils.capitalize(first.getValue()));
        ArgumentCaptor<SetLocaleRequest> captor = ArgumentCaptor.forClass(SetLocaleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(first.getKey(), captor.getValue().getLocaleString());
        verify(userContext).setStringPreference(eq(StringUserPreference.LOCALE), eq(first.getKey()));
    }
}
