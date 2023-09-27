package org.pdfsam.gui.components.sidebar;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/02/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.scene.control.ScrollPane;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.pdfsam.core.ConfigurableSystemProperty;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.gui.components.content.about.AboutContentItem;
import org.pdfsam.gui.components.content.preference.PreferenceContentItem;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.injector.Injector;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadExtension;
import org.testfx.api.FxAssert;

import java.util.Locale;

import static org.hamcrest.CoreMatchers.not;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.testfx.matcher.base.NodeMatchers.hasChild;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class VerticalSidebarTest {

    private Injector injector;

    @BeforeEach
    public void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
        app().persistentSettings().set(BooleanPersistentProperty.SIDEBAR_EXPANDED_STATE, true);
        this.injector = Injector.start(new VerticalSidebarUITest.Config());
    }

    @Test
    @SetSystemProperty(key = ConfigurableSystemProperty.PDFSAM_DISABLE_SETTINGS, value = "true")
    public void preferencesIsDisabled() {
        var bar = injector.instance(VerticalSidebar.class);
        var scroll = (ScrollPane) bar.lookup(".sidebar-scroll");
        var buttons = scroll.getContent();
        FxAssert.verifyThat(buttons, not(hasChild(injector.instance(PreferenceContentItem.class).name())));
        FxAssert.verifyThat(buttons, hasChild(injector.instance(AboutContentItem.class).name()));
    }

    @Test
    @SetSystemProperty(key = ConfigurableSystemProperty.PDFSAM_DISABLE_SETTINGS_DEPRECATED, value = "true")
    public void preferencesIsDisabledDeprecated() {
        var bar = injector.instance(VerticalSidebar.class);
        var scroll = (ScrollPane) bar.lookup(".sidebar-scroll");
        var buttons = scroll.getContent();
        FxAssert.verifyThat(buttons, not(hasChild(injector.instance(PreferenceContentItem.class).name())));
        FxAssert.verifyThat(buttons, hasChild(injector.instance(AboutContentItem.class).name()));
    }
}
