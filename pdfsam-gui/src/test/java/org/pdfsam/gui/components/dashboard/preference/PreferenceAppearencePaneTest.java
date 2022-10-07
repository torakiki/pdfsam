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
package org.pdfsam.gui.components.dashboard.preference;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.ui.ComboItem;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class PreferenceAppearencePaneTest {

    private ApplicationContext appContext = mock(ApplicationContext.class);
    private ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        Locale.setDefault(Locale.ENGLISH);
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
        var localeCombo = new PreferenceComboBox<>(StringPersistentProperty.LOCALE, appContext);
        localeCombo.setId("localeCombo");
        var startupModuleCombo = new PreferenceComboBox<>(StringPersistentProperty.STARTUP_MODULE, appContext);
        PreferenceAppearencePane victim = new PreferenceAppearencePane(localeCombo, startupModuleCombo);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void eventSentOnLocaleChange() {
        Listener<SetLocaleRequest> listener = mock(Listener.class);
        eventStudio().add(SetLocaleRequest.class, listener);
        PreferenceComboBox<ComboItem<String>> localeCombo = robot.lookup("#localeCombo")
                .queryAs(PreferenceComboBox.class);
        var first = localeCombo.getItems().get(0);
        robot.clickOn("#localeCombo").sleep(1000).clickOn(StringUtils.capitalize(first.description()));
        ArgumentCaptor<SetLocaleRequest> captor = ArgumentCaptor.forClass(SetLocaleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(first.key(), captor.getValue().languageTag());
        verify(persistentSettings).set(eq(StringPersistentProperty.LOCALE), eq(first.key()));
    }
}
