/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ott/2014
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
package org.pdfsam.ui.info;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Collections;

import javafx.scene.Parent;
import javafx.scene.control.Button;

import javax.inject.Inject;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataKey;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class InfoStageTest extends GuiTest {
    @Inject
    private ApplicationContext applicationContext;

    @Configuration
    @Lazy
    static class Config {
        @Bean
        public InfoStageController controller() {
            return new InfoStageController();
        }

        @Bean
        public KeywordsTab keywords() {
            return new KeywordsTab();
        }

        @Bean
        public SummaryTab summary() {
            return new SummaryTab();
        }

        @Bean
        public InfoPane pane() {
            return new InfoPane(summary(), keywords());
        }

        @Bean
        public InfoStage stage() {
            return new InfoStage(pane(), Collections.emptyList(), mock(StylesConfig.class));
        }

    }

    @Override
    protected Parent getRootNode() {
        Button button = new Button("show");
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        descriptor.putInformation(PdfMetadataKey.KEYWORDS.getKey(), "test");
        button.setOnAction(e -> eventStudio().broadcast(new ShowPdfDescriptorRequest(descriptor)));
        applicationContext.getBean(InfoStage.class);
        applicationContext.getBean(InfoStageController.class);
        return button;
    }

    @Test
    public void show() {
        click("show");
        InfoStage stage = applicationContext.getBean(InfoStage.class);
        assertTrue(stage.isShowing());
    }

}
