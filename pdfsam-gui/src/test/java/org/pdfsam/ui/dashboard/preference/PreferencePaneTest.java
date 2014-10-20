/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/set/2014
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
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import javax.inject.Inject;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.context.SetLocaleEvent;
import org.pdfsam.context.UserContext;
import org.pdfsam.module.Module;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HighPriorityTestModule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.NewsPolicy;
import org.pdfsam.ui.Theme;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class PreferencePaneTest {

    @ClassRule
    public static ClearEventStudioRule STUDIO_RULE = new ClearEventStudioRule();
    @ClassRule
    public static InitializeAndApplyJavaFxThreadRule INIT_FX = new InitializeAndApplyJavaFxThreadRule();
    @Inject
    private ApplicationContext applicationContext;
    private static UserContext userContext = mock(UserContext.class);

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Configuration
    @Lazy
    @ComponentScan(basePackages = { "org.pdfsam.ui.dashboard.preference" })
    static class Config {
        @Bean
        public UserContext userContext() {
            when(userContext.getTheme()).thenReturn(Theme.ROUNDISH.toString());
            when(userContext.isCheckForUpdates()).thenReturn(Boolean.TRUE);
            when(userContext.isPlaySounds()).thenReturn(Boolean.TRUE);
            when(userContext.isHighQualityThumbnails()).thenReturn(Boolean.TRUE);
            when(userContext.isUseSmartOutput()).thenReturn(Boolean.TRUE);
            when(userContext.getDefaultWorkingPath()).thenReturn("/my/path");
            when(userContext.getDefaultWorkspacePath()).thenReturn("/my/path.xml");
            when(userContext.getThumbnailsSize()).thenReturn(200);
            when(userContext.getStartupModule()).thenReturn("");
            when(userContext.getNewsPolicy()).thenReturn(NewsPolicy.ONCE_A_DAY.toString());
            return userContext;
        }

        @Bean
        public Module aModule() {
            return new HighPriorityTestModule();
        }
    }

    @Test
    @DirtiesContext
    @SuppressWarnings("unchecked")
    public void configOnStartup() {
        PreferencePane victim = applicationContext.getBean(PreferencePane.class);
        PreferenceComboBox<KeyStringValueItem<String>> theme = (PreferenceComboBox<KeyStringValueItem<String>>) victim
                .lookup("#themeCombo");
        PreferenceComboBox<KeyStringValueItem<String>> startupModuleCombo = (PreferenceComboBox<KeyStringValueItem<String>>) victim
                .lookup("#startupModuleCombo");
        PreferenceComboBox<KeyStringValueItem<String>> newsDisplayPolicy = (PreferenceComboBox<KeyStringValueItem<String>>) victim
                .lookup("#newsPolicy");
        assertEquals(Theme.ROUNDISH.friendlyName(), theme.getSelectionModel().getSelectedItem().getValue());
        assertTrue(((PreferenceCheckBox) victim.lookup("#checkForUpdates")).isSelected());
        assertTrue(((PreferenceCheckBox) victim.lookup("#playSounds")).isSelected());
        assertTrue(((PreferenceCheckBox) victim.lookup("#highQualityThumbnails")).isSelected());
        assertTrue(((PreferenceRadioButton) victim.lookup("#smartRadio")).isSelected());
        assertEquals("/my/path.xml", ((PreferenceBrowsableFileField) victim.lookup("#workspace")).getTextField()
                .getText());
        assertEquals("/my/path", ((PreferenceBrowsableDirectoryField) victim.lookup("#workingDirectory"))
                .getTextField().getText());
        assertEquals("200", ((PreferenceIntTextField) victim.lookup("#thumbnailsSize")).getText());
        assertEquals("Dashboard", startupModuleCombo.getSelectionModel().getSelectedItem().getValue());
        assertEquals(NewsPolicy.ONCE_A_DAY.friendlyName(), newsDisplayPolicy.getSelectionModel().getSelectedItem()
                .getValue());
    }

}
