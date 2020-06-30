/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ott/2014
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
package org.pdfsam.ui.info;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Prototype;
import org.pdfsam.injector.Provides;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataFields;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class InfoStageTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    @Components({ InfoStageController.class })
    static class Config {

        @Provides
        StylesConfig style() {
            return mock(StylesConfig.class);
        }

        @Provides
        @Prototype
        public Image payoff() {
            return new Image(this.getClass().getResourceAsStream("/images/payoff.png"));
        }

    }

    @Override
    public void start(Stage stage) {
        injector = Injector.start(new Config());
        Button button = new Button("show");
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        descriptor.putInformation(PdfMetadataFields.KEYWORDS, "test");
        button.setOnAction(e -> eventStudio().broadcast(new ShowPdfDescriptorRequest(descriptor)));
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void show() {
        clickOn("show");
        InfoStage stage = injector.instance(InfoStage.class);
        assertTrue(stage.isShowing());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> stage.hide());
    }

}
