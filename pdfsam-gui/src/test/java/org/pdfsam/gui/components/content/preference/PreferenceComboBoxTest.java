/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
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
package org.pdfsam.gui.components.content.preference;

import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.DefaultPdfVersionComboItem;
import org.sejda.model.pdf.PdfVersion;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */

@ExtendWith({ ApplicationExtension.class })
public class PreferenceComboBoxTest {

    private final ApplicationContext appContext = mock(ApplicationContext.class);
    private final ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
        var first = new PreferenceComboBox<>(StringPersistentProperty.LOCALE, appContext);
        first.setId("first");
        first.getItems().addAll(new ComboItem<>("key1", "value1"), new ComboItem<>("key2", "value2"),
                new ComboItem<>("key3", "value3"));
        var second = new PreferenceComboBox<>(StringPersistentProperty.PDF_VERSION, appContext);
        second.setId("second");
        second.getItems().addAll(new DefaultPdfVersionComboItem(PdfVersion.VERSION_1_3),
                new DefaultPdfVersionComboItem(PdfVersion.VERSION_1_4),
                new DefaultPdfVersionComboItem(PdfVersion.VERSION_1_5));
        Scene scene = new Scene(new VBox(first, second));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void stringPreferenceSetOnClick() {
        robot.clickOn("#first").clickOn("value2");
        verify(persistentSettings).set(eq(StringPersistentProperty.LOCALE), eq("key2"));
    }

    @Test
    @Tag("NoHeadless")
    public void preferenceSetOnClick() {
        robot.clickOn("#second").clickOn(i18n().tr("Version {0}", PdfVersion.VERSION_1_4.getVersionString()));
        verify(persistentSettings).set(eq(StringPersistentProperty.PDF_VERSION), eq(PdfVersion.VERSION_1_4.toString()));
    }

}
