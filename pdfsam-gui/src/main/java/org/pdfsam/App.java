/*
 * Created on 14/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam;

import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.bushe.swing.event.EventBus;
import org.pdfsam.configuration.PdfsamConfig;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.gui.BaseTaskExecutionModule;
import org.pdfsam.gui.MainFrame;
import org.pdfsam.gui.WelcomePanel;
import org.pdfsam.gui.event.TaskExecutionModulesLoadedEvent;
import org.pdfsam.gui.menu.MenuType;
import org.pdfsam.gui.support.SwingUtils;
import org.pdfsam.update.UpdateCheckRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import bibliothek.gui.dock.common.CControl;
import bibliothek.gui.dock.common.theme.ThemeMap;
import bibliothek.gui.dock.util.IconManager;

/**
 * Entry point to start pdfsam
 * 
 * @author Andrea Vacondio
 * 
 */
public final class App {

    private static final Logger LOG = LoggerFactory.getLogger(App.class);
    private static MainFrame mainFrame = null;
    private static AnnotationConfigApplicationContext ctx;

    private App() {
        // hide
    }

    public static void main(String[] args) throws Exception {
        LOG.info(DefaultI18nContext.getInstance().i18n("Starting pdfsam"));
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        try {
            initLookAndFeel();
            mainFrame = new MainFrame();
            CControl control = new CControl(mainFrame);
            control.setTheme(ThemeMap.KEY_FLAT_THEME);
            initIcons(control);
            mainFrame.initControl(control);
            mainFrame.addSystemContentAction(MenuType.HELP, new WelcomePanel());

            initIoC();
            SwingUtils.centrePositionOnScreen(mainFrame);
            mainFrame.setVisible(true);
            requestCheckForUpdateIfNecessary();
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().i18n("Error starting pdfsam."), e);
            throw e;
        }
        stopWatch.stop();
        LOG.info(DefaultI18nContext.getInstance().i18n("Started in {0}",
                DurationFormatUtils.formatDurationWords(stopWatch.getTime(), true, true)));
    }

    private static void requestCheckForUpdateIfNecessary() {
        if (DefaultUserContext.getInstance().isCheckForUpdates()) {
            EventBus.publish(new UpdateCheckRequest());
        }
    }

    private static void initIcons(CControl control) {
        IconManager icons = control.getController().getIcons();
        icons.setIconClient("locationmanager.normalize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/normalize.png")));
        icons.setIconClient("locationmanager.maximize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/maximize.png")));
        icons.setIconClient("locationmanager.minimize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/minimize.png")));
        icons.setIconClient("locationmanager.externalize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/externalize.png")));
        icons.setIconClient(
                "locationmanager.unexternalize",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/unexternalize.png")));
        icons.setIconClient("locationmanager.unmaximize_externalized",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/common/icons/eclipse/normalize.png")));
        icons.setIconClient("flap.hold",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/core/eclipse/holdon.png")));
        icons.setIconClient("flap.free",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/core/eclipse/holdoff.png")));
        icons.setIconClient("close",
                new ImageIcon(App.class.getResource("/data/bibliothek/gui/dock/core/eclipse/closex.png")));
    }

    private static void initLookAndFeel() throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        try {
            UIManager.setLookAndFeel(DefaultUserContext.getInstance().getLookAndFeelClass());
        } catch (UnsupportedLookAndFeelException e) {
            LOG.warn(DefaultI18nContext.getInstance().i18n(
                    "Unable to install the selected look and feel because it's unsupported."));
        }

    }

    private static void initIoC() {
        ctx = new AnnotationConfigApplicationContext(PdfsamConfig.class);
        Map<String, BaseTaskExecutionModule> modules = ctx.getBeansOfType(BaseTaskExecutionModule.class);
        LOG.debug("Found {} modules", modules.size());
        TaskExecutionModulesLoadedEvent initModulesEvent = new TaskExecutionModulesLoadedEvent();
        initModulesEvent.addAll(modules.values());
        EventBus.publish(initModulesEvent);
        ctx.registerShutdownHook();
    }

    /**
     * @return the mainFrame
     */
    public static MainFrame getMainFrame() {
        return mainFrame;
    }

}
