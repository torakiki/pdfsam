/*
 * This file is part of the PDF Split And Merge source code
 * Created on 01/set/2014
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

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HighPriorityTestTool;
import org.pdfsam.test.JavaFxThreadExtension;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class PreferencePaneTest {
    private Injector injector;

    @BeforeAll
    public static void setUpClass() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
        app().persistentSettings().set(BooleanPersistentProperty.CHECK_UPDATES, Boolean.TRUE);
        app().persistentSettings().set(BooleanPersistentProperty.CHECK_FOR_NEWS, Boolean.TRUE);
        app().persistentSettings().set(BooleanPersistentProperty.PLAY_SOUNDS, Boolean.FALSE);
        app().persistentSettings().set(BooleanPersistentProperty.DONATION_NOTIFICATION, Boolean.TRUE);
        app().persistentSettings().set(BooleanPersistentProperty.PREMIUM_MODULES, Boolean.TRUE);
        app().persistentSettings().set(BooleanPersistentProperty.SMART_OUTPUT, Boolean.TRUE);
        app().persistentSettings().set(StringPersistentProperty.WORKING_PATH, "/my/path");
        app().persistentSettings().set(StringPersistentProperty.WORKSPACE_PATH, "/my/path.xml");
        app().persistentSettings().set(StringPersistentProperty.STARTUP_MODULE, "");
    }

    @AfterAll
    public static void tearDown() {
        app().clean();
    }

    @BeforeEach
    public void setUp() {
        injector = Injector.start(new Config(), new PreferenceConfig());
    }

    static class Config {
        @Provides
        public Tool aModule() {
            return new HighPriorityTestTool();
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void configOnStartup() {
        PreferencePane victim = injector.instance(PreferencePane.class);
        var startupModuleCombo = (PreferenceComboBox<ComboItem<String>>) victim.lookup("#startupModuleCombo");
        assertTrue(((PreferenceCheckBox) victim.lookup("#checkForUpdates")).isSelected());
        assertTrue(((PreferenceCheckBox) victim.lookup("#checkForNews")).isSelected());
        assertFalse(((PreferenceCheckBox) victim.lookup("#playSounds")).isSelected());
        assertTrue(((PreferenceCheckBox) victim.lookup("#donationNotification")).isSelected());
        assertTrue(((PreferenceCheckBox) victim.lookup("#fetchPremiumModules")).isSelected());
        assertTrue(((PreferenceRadioButton) victim.lookup("#smartRadio")).isSelected());
        assertEquals("/my/path.xml",
                ((PreferenceBrowsableFileField) victim.lookup("#workspace")).getTextField().getText());
        assertEquals("/my/path",
                ((PreferenceBrowsableDirectoryField) victim.lookup("#workingDirectory")).getTextField().getText());
        assertEquals(i18n().tr("Dashboard"), startupModuleCombo.getSelectionModel().getSelectedItem().description());
    }

}
